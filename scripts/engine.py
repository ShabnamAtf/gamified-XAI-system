from pathlib import Path
import yaml, json
import pandas as pd
import numpy as np
import operator

ROOT = Path(__file__).resolve().parents[1]
STIM = ROOT / "stimuli"

def _load_yaml(p): 
    return yaml.safe_load(open(p, "r", encoding="utf-8"))

def load_resources():
    scen = pd.read_csv(STIM / "scenarios.csv")
    schema = json.load(open(STIM / "attribute_schema.json","r",encoding="utf-8"))
    rules = _load_yaml(STIM / "kantian_rules.yml")
    uw = _load_yaml(STIM / "utilitarian_weights.yml")
    cfg = _load_yaml(STIM / "experiment_config.yml")
    try:
        templates = pd.read_csv(STIM / "explanation_templates.csv")
    except Exception:
        templates = pd.DataFrame(columns=["condition","rule_id","template_text"])
    return scen, schema, rules, uw, cfg, templates

def util_attrs(uw):
    if "weights" in uw: 
        return list(uw["weights"].keys())
    # support weight_sets
    return list(next(iter(uw["weight_sets"]))["weights"].keys())

def minmax_by_scenario(df, cols):
    out = df.copy()
    for sid, g in df.groupby("scenario_id"):
        for c in cols:
            mn, mx = g[c].min(), g[c].max()
            rng = (mx - mn) + 1e-12
            out.loc[g.index, c+"_norm"] = (g[c] - mn) / rng
    return out

def util_score(df, uw):
    if "weights" in uw:
        weights = uw["weights"]
    else:
        weights = next(iter({w["id"]:w["weights"] for w in uw["weight_sets"]}.values()))
    s = 0.0
    for k, w in weights.items():
        s = s + df[f"{k}_norm"].astype(float)*float(w)
    return s

def _op_fn(sym):
    return {"<":operator.lt, "<=":operator.le, ">":operator.gt, ">=":operator.ge, "==":operator.eq}.get(sym, operator.lt)

def kantian_eval_row(row, rules):
    severities = []
    msgs = []
    for r in rules.get("rules", []):
        attr = r["attribute"]; op = r.get("op","<"); thr = r["threshold"]; sev = int(r.get("severity",1))
        if attr not in row: 
            continue
        if _op_fn(op)(row[attr], thr):
            severities.append(sev)
            msgs.append(r.get("message_id", r.get("id", "RULE")))
    return (sum(severities) if severities else 0), msgs

def explain_kantian(msgs, templates):
    if templates.empty: 
        return "Flags deontic rule violations: " + (", ".join(msgs) if msgs else "none.")
    t = templates[templates["condition"].eq("kantian")]
    if t.empty: 
        return "Flags deontic rule violations: " + (", ".join(msgs) if msgs else "none.")
    return "; ".join(
        t.loc[t["rule_id"].isin(msgs), "template_text"].tolist()
    ) or "No rule violation."

def explain_utilitarian(row, uw):
    if "weights" in uw: 
        weights = uw["weights"]
    else:
        weights = next(iter({w["id"]:w["weights"] for w in uw["weight_sets"]}.values()))
    parts = []
    for k,w in weights.items():
        parts.append(f"{k}×{w:+.2f}→{row[f'{k}_norm']*w:+.3f}")
    return "MCDA: " + ", ".join(parts) + f" | total={row['util_score']:+.3f}"

def meta_pick(df_scene, delta_frac=0.33):
    best_idx = df_scene["util_score"].idxmax()
    best = df_scene.loc[best_idx]
    if best["kantian_severity_sum"] == 0:
        return best
    mn, mx = df_scene["util_score"].min(), df_scene["util_score"].max()
    rng = (mx - mn) + 1e-12
    thr = best["util_score"] - delta_frac*rng
    ok = df_scene[df_scene["kantian_severity_sum"].eq(0)]
    if ok.empty:
        return best
    near = ok[ ok["util_score"] >= thr ]
    if near.empty:
        return best
    return near.loc[near["util_score"].idxmax()]

def prepare_scene(scen_all, schema, rules, uw, scenario_id):
    attrs = util_attrs(uw)
    df = scen_all[scen_all["scenario_id"].eq(scenario_id)].copy()
    df = minmax_by_scenario(df, attrs)
    df["util_score"] = util_score(df, uw)
    ks = []; msgs = []
    for _,r in df.iterrows():
        sev, mm = kantian_eval_row(r, rules)
        ks.append(sev); msgs.append(mm)
    df["kantian_severity_sum"] = ks
    df["kantian_msgs"] = msgs
    return df

def decide(df_scene, policy="utilitarian", delta_frac=0.33):
    if policy=="none":
        # emulate price-driven baseline
        return df_scene.loc[df_scene["price_cad"].idxmin()]
    if policy=="utilitarian":
        return df_scene.loc[df_scene["util_score"].idxmax()]
    if policy=="kantian":
        ok = df_scene[df_scene["kantian_severity_sum"].eq(0)]
        if ok.empty:
            # least severe, tie-break by util
            m = df_scene["kantian_severity_sum"].min()
            tie = df_scene[df_scene["kantian_severity_sum"].eq(m)]
            return tie.loc[tie["util_score"].idxmax()]
        return ok.loc[ok["util_score"].idxmax()]
    if policy=="combined_meta":
        return meta_pick(df_scene, delta_frac=delta_frac)
    return df_scene.iloc[0]

