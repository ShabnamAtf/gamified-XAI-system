# scripts/build_condition_table.py
# -*- coding: utf-8 -*-
"""
Builds the Condition summary table from outputs/ CSVs.

Expected (but flexible) inputs under outputs/:
- decision_meta_config.csv     → columns like: condition, round_id, regret, switched, delta_vs_price (optional)
- policy_trace_text.csv        → columns like: condition, round_id, option_id, selected, kantian_status, violations
- decision_trace_config.csv    → optional, if you store selection there instead of policy_trace_text
- utilitarian_scores.csv       → optional, only used if delta_vs_price isn't provided

It writes: outputs/condition_table.csv
"""

from pathlib import Path
import pandas as pd
import numpy as np
import glob
import re

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "outputs"
OUT.mkdir(exist_ok=True, parents=True)

def smart_read_one(patterns):
    """Try multiple glob patterns; return first loadable DataFrame or None."""
    for pat in patterns:
        for f in sorted(glob.glob(str(OUT / pat))):
            try:
                df = pd.read_csv(f)
                print(f"[loaded] {f}  shape={df.shape}")
                return df
            except Exception as e:
                print(f"[warn] couldn't read {f}: {e}")
    return None

def ensure_condition(df, default_name):
    """Ensure a 'condition' column exists; if not, inject a default."""
    if df is None:
        return None
    if "condition" not in df.columns:
        df = df.copy()
        df["condition"] = default_name
    return df

def parse_severity(violations_col):
    """
    Parse a 'violations' string like 'r1;r3' or 'r1|r3' or '' into an integer count.
    If weights are embedded like 'r1:2;r3:1', sum the weights.
    """
    if pd.isna(violations_col) or str(violations_col).strip().lower() in ("", "none", "null"):
        return 0
    s = str(violations_col)
    # split by common separators
    parts = re.split(r"[;,\|]+", s)
    total = 0
    for p in parts:
        p = p.strip()
        if not p:
            continue
        if ":" in p:
            # form 'r1:2'
            try:
                w = float(p.split(":", 1)[1])
            except Exception:
                w = 1.0
            total += w
        else:
            total += 1
    return total

def compute_violation_stats(trace_df):
    """
    From policy_trace_text (or decision_trace) compute:
      - Violation-free (%) among SELECTED rows
      - Mean Kantian severity (average severity across SELECTED rows)
    """
    df = trace_df.copy()

    # Figure out selected column; fallbacks
    sel_col = None
    for c in ["selected", "is_selected", "chosen", "pick"]:
        if c in df.columns:
            sel_col = c
            break
    if sel_col is None:
        raise ValueError("No 'selected' flag found in trace file. Expected one of [selected|is_selected|chosen|pick].")

    # Normalize kantian status column
    if "kantian_status" not in df.columns:
        # try alternative names
        for c in ["kantian", "deontic_status", "kantian_clean"]:
            if c in df.columns:
                df["kantian_status"] = df[c]
                break
    if "kantian_status" not in df.columns:
        # if still missing, infer from violations == 0 later
        df["kantian_status"] = np.nan

    # Parse severity from 'violations' (count or weight-sum)
    if "violations" not in df.columns:
        df["violations"] = ""
    df["kantian_severity"] = df["violations"].apply(parse_severity)

    # If kantian_status missing, infer clean if severity==0
    mask_missing = df["kantian_status"].isna()
    if mask_missing.any():
        df.loc[mask_missing, "kantian_status"] = np.where(
            df.loc[mask_missing, "kantian_severity"] == 0, "clean", "violation"
        )

    sel = df[df[sel_col].astype(bool)].copy()
    if sel.empty:
        raise ValueError("No selected rows found in trace file.")

    # group by condition
    def _agg(g):
        n = len(g)
        clean_n = (g["kantian_status"].str.lower() == "clean").sum()
        viol_free_pct = 100.0 * clean_n / n if n else np.nan
        mean_sev = g["kantian_severity"].mean() if n else np.nan
        return pd.Series({
            "Violation-free (%)": viol_free_pct,
            "Mean Kantian severity": mean_sev,
            "n_selected": n
        })

    stats = sel.groupby("condition", dropna=False).apply(_agg).reset_index()
    return stats

def compute_delta_and_conflict(meta_df, util_df=None):
    """
    Compute:
      - Δ welfare vs. price  (preferred): mean of 'delta_vs_price' if present.
        Fallback: use negative mean 'regret' (–regret vs util_best) as a proxy (higher is better).
      - Conflict resolved (%) for Combined+Meta: 100 * mean(switched)
        Other conditions get NaN.
    """
    df = meta_df.copy()

    # Ensure numeric
    for c in ["regret", "util_best_score", "delta_vs_price", "switched"]:
        if c in df.columns:
            # coerce numeric/bool
            if c == "switched":
                df[c] = df[c].astype(str).str.lower().isin(["1", "true", "yes"])
            else:
                df[c] = pd.to_numeric(df[c], errors="coerce")

    # Δ welfare vs price
    delta_by_cond = None
    if "delta_vs_price" in df.columns and df["delta_vs_price"].notna().any():
        delta_by_cond = df.groupby("condition")["delta_vs_price"].mean().rename("Δ welfare vs. price").reset_index()
    elif "regret" in df.columns and df["regret"].notna().any():
        # fallback proxy: –mean(regret) (closer to util-best is better)
        tmp = (-df["regret"]).groupby(df["condition"]).mean().rename("Δ welfare vs. price").reset_index()
        delta_by_cond = tmp
    else:
        # last-resort: try to estimate from util_df if available (rare)
        if util_df is not None and {"condition", "round_id", "option_id", "util_score", "price_cad_per_cup", "selected"}.issubset(util_df.columns):
            u = util_df.copy()
            # find selected util score & price-min option util score per (cond, round)
            sel = u[u["selected"].astype(bool)]
            price_min = u.loc[u.groupby(["condition","round_id"])["price_cad_per_cup"].idxmin()]
            j = sel.merge(price_min[["condition","round_id","util_score"]].rename(columns={"util_score":"util_price_min"}),
                          on=["condition","round_id"], how="left")
            j["delta_vs_price_est"] = j["util_score"] - j["util_price_min"]
            delta_by_cond = j.groupby("condition")["delta_vs_price_est"].mean().rename("Δ welfare vs. price").reset_index()
        else:
            raise ValueError("Cannot compute 'Δ welfare vs. price': no delta_vs_price, no regret, and insufficient utilitarian_scores.")

    # Conflict resolved (%)
    conflict = None
    if "switched" in df.columns:
        c = df.groupby("condition")["switched"].mean().mul(100.0).rename("Conflict resolved (%)").reset_index()
        conflict = c
    else:
        # If absent, fill NaN to be safe
        conds = df["condition"].dropna().unique()
        conflict = pd.DataFrame({"condition": conds, "Conflict resolved (%)": np.nan})

    out = delta_by_cond.merge(conflict, on="condition", how="outer")
    return out

def main():
    # Load meta
    meta = smart_read_one(["decision_meta_config*.csv", "decision_meta*.csv"])
    if meta is None:
        raise FileNotFoundError("decision_meta_config*.csv not found under outputs/")
    meta = ensure_condition(meta, default_name="combined_meta")

    # Load trace (for Kantian stats)
    trace = smart_read_one(["policy_trace_text*.csv", "decision_trace_config*.csv"])
    if trace is None:
        raise FileNotFoundError("policy_trace_text*.csv (or decision_trace_config*.csv) not found under outputs/")
    trace = ensure_condition(trace, default_name="combined_meta")

    # Optional utilitarian scores (only needed in rare fallback)
    util = smart_read_one(["utilitarian_scores*.csv"])
    if util is not None:
        util = ensure_condition(util, default_name="combined_meta")

    # Compute parts
    kantian_stats = compute_violation_stats(trace)
    delta_conflict = compute_delta_and_conflict(meta, util_df=util)

    # Merge
    summary = kantian_stats.merge(delta_conflict, on="condition", how="outer")

    # Beautify + order + formats
    # Known condition labels (map if you use codes)
    rename_map = {
        "no_expl": "No explanation",
        "kantian_only": "Kantian-only",
        "util_only": "Utilitarian-only",
        "combined_meta": "Combined + Meta",
        "combined+meta": "Combined + Meta",
        "combined_meta_expl": "Combined + Meta",
    }
    summary["Condition"] = summary["condition"].replace(rename_map).fillna(summary["condition"])
    summary = summary.drop(columns=["condition"])

    # round/format columns
    def fmt_pct(x):
        return np.nan if pd.isna(x) else round(float(x), 1)

    def fmt_f(x, nd=3):
        return np.nan if pd.isna(x) else round(float(x), nd)

    summary["Δ welfare vs. price"] = summary["Δ welfare vs. price"].apply(lambda v: fmt_f(v, 3))
    summary["Violation-free (%)"] = summary["Violation-free (%)"].apply(fmt_pct)
    summary["Mean Kantian severity"] = summary["Mean Kantian severity"].apply(lambda v: fmt_f(v, 2))
    summary["Conflict resolved (%)"] = summary["Conflict resolved (%)"].apply(fmt_pct)

    # Sort by a preferred order if present
    order = ["No explanation", "Kantian-only", "Utilitarian-only", "Combined + Meta"]
    summary["__order"] = summary["Condition"].apply(lambda c: order.index(c) if c in order else 999)
    summary = summary.sort_values(["__order", "Condition"]).drop(columns="__order")

    # Save
    out_csv = OUT / "condition_table.csv"
    summary.to_csv(out_csv, index=False)
    print("\n[done] wrote:", out_csv)
    print(summary.to_string(index=False))

if __name__ == "__main__":
    main()

