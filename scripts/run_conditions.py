from pathlib import Path
import sys
import argparse
import yaml
import pandas as pd
import numpy as np

ROOT = Path(__file__).resolve().parents[1]
OUT_DIR = ROOT / "outputs"

def load_inputs():
    ucfg = yaml.safe_load(open(ROOT / "stimuli/utilitarian_weights.yml", "r", encoding="utf-8"))
    sc_path = OUT_DIR / "options_scored.csv"
    if not sc_path.exists():
        sys.exit(f"[ERROR] Missing {sc_path}. Run scripts/run_experiment.py first.")
    sc = pd.read_csv(sc_path)
    required = {"scenario_id", "util_score", "kantian_severity_sum", "price_cad"}
    missing = required - set(sc.columns)
    if missing:
        sys.exit(f"[ERROR] Missing required columns in options_scored.csv: {sorted(missing)}")
    return ucfg, sc

def weight_sets(ucfg):
    if "weight_sets" in ucfg and ucfg["weight_sets"]:
        return {ws["id"]: ws["weights"] for ws in ucfg["weight_sets"]}
    return {"default": ucfg["weights"]}

def score_by_weights(df, weights):
    s = pd.Series(0.0, index=df.index)
    for k, w in weights.items():
        col = f"{k}_norm"
        if col not in df.columns:
            sys.exit(f"[ERROR] Missing normalized column: {col}")
        s = s + df[col].astype(float) * float(w)
    return s

def choose_none(df, weights):
    tmp = df.assign(_scr=score_by_weights(df, weights))
    return tmp.loc[tmp.groupby("scenario_id")["_scr"].idxmax()].drop(columns="_scr", errors="ignore")

def choose_utilitarian(df):
    return df.loc[df.groupby("scenario_id")["util_score"].idxmax()]

def choose_kantian(df, weights):
    ok = df[df["kantian_severity_sum"] == 0]
    if not ok.empty:
        tmp = ok.assign(_scr=score_by_weights(ok, weights))
        return tmp.loc[tmp.groupby("scenario_id")["_scr"].idxmax()].drop(columns="_scr", errors="ignore")
    minsev = df.groupby("scenario_id")["kantian_severity_sum"].transform("min")
    tie = df[df["kantian_severity_sum"] == minsev]
    tmp = tie.assign(_scr=score_by_weights(tie, weights))
    return tmp.loc[tmp.groupby("scenario_id")["_scr"].idxmax()].drop(columns="_scr", errors="ignore")

def choose_combined_meta(df, weights, delta_util):
    tmp = df.assign(_scr=score_by_weights(df, weights))
    best_personal = tmp.loc[tmp.groupby("scenario_id")["_scr"].idxmax()]
    best_util = tmp.loc[tmp.groupby("scenario_id")["util_score"].idxmax()][["scenario_id", "util_score"]]
    best_personal = best_personal.merge(best_util.rename(columns={"util_score": "best_util"}), on="scenario_id", how="left")
    ok = tmp[tmp["kantian_severity_sum"] == 0].copy()
    if ok.empty:
        return best_personal.drop(columns="_scr", errors="ignore")
    ok_best = ok.loc[ok.groupby("scenario_id")["util_score"].idxmax()][["scenario_id", "util_score"]]
    ok_best = ok_best.rename(columns={"util_score": "ok_util"})
    m = best_personal.merge(ok_best, on="scenario_id", how="left")
    choose_ok = (m["kantian_severity_sum"] > 0) & ((m["best_util"] - m["ok_util"]) <= delta_util)
    result = best_personal.set_index("scenario_id")
    repl = ok.loc[ok.groupby("scenario_id")["util_score"].idxmax()].set_index("scenario_id")
    idx = m.loc[choose_ok, "scenario_id"].values
    result.update(repl[repl.index.isin(idx)])
    return result.reset_index().drop(columns="_scr", errors="ignore")

def pick_min_price(df):
    return df.loc[df.groupby("scenario_id")["price_cad"].idxmin()]

def summarize(choices, baseline):
    util = choices["util_score"].mean()
    kant = choices["kantian_severity_sum"].mean()
    vfree = (choices["kantian_severity_sum"] == 0).mean()
    base = baseline[["scenario_id", "util_score"]].set_index("scenario_id")
    ch = choices[["scenario_id", "util_score"]].set_index("scenario_id")
    uplift = (ch["util_score"] - base["util_score"]).mean()
    return {
        "mean_util": round(util, 3),
        "mean_kantian_severity": round(kant, 3),
        "violation_free_share": round(vfree, 3),
        "welfare_uplift_vs_price": round(uplift, 3),
    }

def conflict_resolution(per_w, df, delta_frac):
    best = choose_utilitarian(df.copy())[["scenario_id", "kantian_severity_sum", "util_score"]].rename(
        columns={"kantian_severity_sum": "kantian_severity_sum_best", "util_score": "util_score_best"}
    )
    cm_all = pd.concat([per_w[w]["combined_meta"] for w in per_w], ignore_index=True)[
        ["scenario_id", "kantian_severity_sum", "util_score"]
    ].rename(columns={"kantian_severity_sum": "kantian_severity_sum_cm", "util_score": "util_score_cm"})
    merged = cm_all.merge(best, on="scenario_id", how="left")

    util_range = df.groupby("scenario_id")["util_score"].agg(lambda s: s.max() - s.min()).rename("util_range")
    merged = merged.merge(util_range, on="scenario_id", how="left")
    merged["util_range"] = merged["util_range"].replace(0, np.nan).fillna(1e-12)

    merged["gap_frac"] = (merged["util_score_best"] - merged["util_score_cm"]) / merged["util_range"]

    is_conflict = merged["kantian_severity_sum_best"] > 0
    is_resolved = (merged["kantian_severity_sum_cm"] == 0) & (merged["gap_frac"] <= delta_frac)

    scenario_resolved = merged.assign(resolved=(is_conflict & is_resolved)).groupby("scenario_id")["resolved"].any()
    return float(scenario_resolved.mean())

def run_once(delta_util, delta_frac):
    ucfg, sc = load_inputs()
    wsets = weight_sets(ucfg)
    baseline = pick_min_price(sc)

    per_w = {}
    for wid, w in wsets.items():
        per_w[wid] = {
            "none": choose_none(sc.copy(), w),
            "utilitarian": choose_utilitarian(sc.copy()),
            "kantian": choose_kantian(sc.copy(), w),
            "combined_meta": choose_combined_meta(sc.copy(), w, delta_util=delta_util),
        }

    summary_rows = []
    for cond in ["none", "kantian", "utilitarian", "combined_meta"]:
        all_w = pd.concat([per_w[wid][cond] for wid in per_w], ignore_index=True)
        base_rep = pd.concat([baseline] * len(per_w), ignore_index=True)
        met = summarize(all_w, base_rep)
        met["condition"] = cond
        summary_rows.append(met)

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(summary_rows).to_csv(OUT_DIR / "condition_summary.csv", index=False)

    rate = conflict_resolution(per_w, sc, delta_frac=delta_frac)
    pd.DataFrame([{"metric": "conflict_resolution_rate", "value": round(rate, 3)}]).to_csv(
        OUT_DIR / "condition_conflict_resolution.csv", index=False
    )

    print("Saved:", OUT_DIR / "condition_summary.csv", "and", OUT_DIR / "condition_conflict_resolution.csv")

def sweep(deltas, k=1.5):
    ucfg, sc = load_inputs()
    wsets = weight_sets(ucfg)
    rows = []
    for du in deltas:
        dn = k * du
        per_w = {}
        for wid, w in wsets.items():
            per_w[wid] = {
                "none": choose_none(sc.copy(), w),
                "utilitarian": choose_utilitarian(sc.copy()),
                "kantian": choose_kantian(sc.copy(), w),
                "combined_meta": choose_combined_meta(sc.copy(), w, delta_util=du),
            }
        baseline = pick_min_price(sc)
        all_w = pd.concat([per_w[wid]["combined_meta"] for wid in per_w], ignore_index=True)
        base_rep = pd.concat([baseline] * len(per_w), ignore_index=True)
        met = summarize(all_w, base_rep)
        rate = conflict_resolution(per_w, sc, delta_frac=dn)
        met.update({"delta_util": du, "delta_frac": dn, "conflict_resolution_rate": round(rate, 3)})
        rows.append(met)
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(rows).to_csv(OUT_DIR / "meta_sweep.csv", index=False)
    print("Saved:", OUT_DIR / "meta_sweep.csv")

def parse_args():
    p = argparse.ArgumentParser()
    p.add_argument("--delta-util", type=float, default=0.22)
    p.add_argument("--delta-frac", type=float, default=0.33)
    p.add_argument("--sweep", type=str, default="")
    return p.parse_args()

def main():
    args = parse_args()
    if args.sweep.strip():
        deltas = [float(x) for x in args.sweep.split(",")]
        sweep(deltas)
    run_once(args.delta_util, args.delta_frac)
    print("Done")

if __name__ == "__main__":
    main()

