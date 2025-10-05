from pathlib import Path
import argparse, json
import yaml
import numpy as np
import pandas as pd

def load_cfg(cfg_path: Path):
    with open(cfg_path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)

def resolve_paths(root: Path, cfg):
    p = {}
    for k, rel in cfg["paths"].items():
        p[k] = (root / rel).resolve()
    return p

def minmax_by_scenario(df, attr_meta):
    df = df.copy()
    # build helpers for direction
    goals = {k: v["goal"] for k, v in attr_meta.items()}
    attrs = list(attr_meta.keys())
    # per-scenario normalization
    def norm_group(g):
        for col in attrs:
            if col not in g.columns: 
                continue
            vals = g[col].astype(float)
            mn, mx = vals.min(), vals.max()
            if np.isclose(mx, mn):
                x = np.full_like(vals, 0.5, dtype=float)
            else:
                x = (vals - mn) / (mx - mn)
            # flip if lower_better
            if goals[col] == "lower_better":
                x = 1.0 - x
            g[col + "_norm"] = x.round(3)
        return g
    return df.groupby("scenario_id", group_keys=False).apply(norm_group)

def get_weights(weights_cfg, util_cfg):
    # prefer a named weight_set if provided
    if "weight_sets" in weights_cfg and util_cfg.get("weight_set_id"):
        for ws in weights_cfg["weight_sets"]:
            if ws["id"] == util_cfg["weight_set_id"]:
                return ws["weights"]
    # else fall back to top-level weights
    return weights_cfg["weights"]

def utilitarian_score(df_norm, weights):
    df = df_norm.copy()
    # ensure all needed columns exist
    for k in list(weights.keys()):
        col = k + "_norm"
        if col not in df.columns:
            raise ValueError(f"Missing normalized column: {col}")
    w = pd.Series(weights, dtype=float)
    # weighted sum over normalized columns
    util = sum(df[k + "_norm"] * float(w[k]) for k in w.index)
    df["util_score"] = util.round(3)
    return df

def apply_kantian(df, rules_cfg):
    df = df.copy()
    rules = rules_cfg.get("rules", [])
    sev_w = {int(k): float(v) for k, v in rules_cfg.get("meta", {}).get("severity_weights", {"1":1,"2":2,"3":3}).items()}
    def check_rule(row, r):
        v = float(row[r["attribute"]])
        thr = float(r["threshold"])
        op = r["op"].strip()
        if op == "<": ok = v < thr
        elif op == "<=": ok = v <= thr
        elif op == ">": ok = v > thr
        elif op == ">=": ok = v >= thr
        elif op == "==": ok = np.isclose(v, thr)
        elif op == "!=": ok = not np.isclose(v, thr)
        else: raise ValueError(f"Unknown op: {op}")
        return ok

    viol_list = []
    viol_sev = []
    for _, row in df.iterrows():
        fired = []
        total = 0.0
        for r in rules:
            if check_rule(row, r):
                fired.append(r["id"])
                total += sev_w.get(int(r.get("severity",1)), 1.0)
        viol_list.append(", ".join(fired))
        viol_sev.append(round(total, 3))
    df["kantian_violations"] = viol_list
    df["kantian_severity_sum"] = viol_sev
    return df

def rank_within_scenario(df):
    df = df.copy()
    df["util_rank"] = df.groupby("scenario_id")["util_score"].rank(ascending=False, method="dense").astype(int)
    # deltas to best in scenario
    best = df.groupby("scenario_id")["util_score"].transform("max")
    df["delta_to_best"] = (df["util_score"] - best).round(3)
    return df

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--config", default="stimuli/experiment_config.yml")
    args = ap.parse_args()

    root = Path(__file__).resolve().parents[1]
    cfg = load_cfg(root / args.config)
    paths = resolve_paths(root, cfg)

    # load inputs
    scen = pd.read_csv(paths["scenarios"])
    with open(paths["attribute_schema"], "r", encoding="utf-8") as f: schema = json.load(f)
    with open(paths["kantian_rules"], "r", encoding="utf-8") as f: k_rules = yaml.safe_load(f)
    with open(paths["utilitarian_weights"], "r", encoding="utf-8") as f: u_w = yaml.safe_load(f)

    # normalize per scenario
    df = minmax_by_scenario(scen, schema["attributes"])
    # utilitarian score
    weights = get_weights(u_w, cfg.get("utilitarian", {}))
    df = utilitarian_score(df, weights)
    # kantian violations
    df = apply_kantian(df, k_rules)
    # ranking & delta
    df = rank_within_scenario(df)

    # select nice columns for export
    base_cols = ["scenario_id","product_id","product_name","category",
                 "price_cad","carbon_kgco2_perkg","animal_welfare_0_100",
                 "transparency_0_100","packaging_recyclability_pct","labour_community_0_100"]
    extra_cols = [c for c in df.columns if c.endswith("_norm")] + ["util_score","util_rank","delta_to_best","kantian_severity_sum","kantian_violations"]
    out_df = df[base_cols + extra_cols].sort_values(["scenario_id","util_rank"])

    # summaries for the poster
    summary = pd.DataFrame({
        "scenarios": [df["scenario_id"].nunique()],
        "options": [len(df)],
        "mean_util": [round(df["util_score"].mean(), 3)],
        "median_util": [round(df["util_score"].median(), 3)],
        "mean_kantian_severity": [round(df["kantian_severity_sum"].mean(), 3)],
        "violation_free_options": [(df["kantian_severity_sum"] == 0).sum()]
    })

    # write outputs
    out_dir = (root / cfg.get("output", {}).get("dir", "outputs")).resolve()
    out_dir.mkdir(parents=True, exist_ok=True)
    out_df.to_csv(out_dir / "options_scored.csv", index=False)
    summary.to_csv(out_dir / "summary_overall.csv", index=False)

    print(f"Saved: {out_dir / 'options_scored.csv'}")
    print(f"Saved: {out_dir / 'summary_overall.csv'}")
    print("Done ✅")

if __name__ == "__main__":
    main()

