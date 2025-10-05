# scripts/build_condition_table.py
# -*- coding: utf-8 -*-
from pathlib import Path
import pandas as pd, numpy as np, glob, re

ROOT = Path(__file__).resolve().parents[1]   # .../Logic
OUT  = ROOT / "outputs"
OUT.mkdir(exist_ok=True, parents=True)

# -------------------- utilities --------------------

def smart_read_one(patterns):
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
    if df is None: return None
    df = df.copy()
    if "condition" not in df.columns:
        df["condition"] = default_name
    return df

def std_round_col(df):
    """Return name of round column and ensure integer."""
    for c in ["round_idx", "round_id", "round", "rnd"]:
        if c in df.columns:
            col = c
            break
    else:
        raise ValueError("No round column found (expected one of: round_idx|round_id|round|rnd).")
    df[col] = pd.to_numeric(df[col], errors="coerce").astype("Int64")
    return df, col

def std_option_id(df):
    """Return name of option_id column and ensure string."""
    for c in ["option_id", "opt_id", "id"]:
        if c in df.columns:
            col = c
            break
    else:
        # last-resort: if only labels exist, we'll merge on labels later
        return df, None
    df[col] = df[col].astype(str)
    return df, col

def std_option_label(df):
    """Return name of option_label column if present."""
    for c in ["option_label", "label", "letter"]:
        if c in df.columns:
            return df, c
    return df, None

def sev_from_violations(s):
    if pd.isna(s) or str(s).strip().lower() in ("", "none", "null"): return 0.0
    tot = 0.0
    for p in re.split(r"[;,\|]+", str(s)):
        p = p.strip()
        if not p: continue
        if ":" in p:
            try: tot += float(p.split(":",1)[1])
            except: tot += 1.0
        else: tot += 1.0
    return tot

def sev_from_rule_flags(row):
    rule_cols = [c for c in row.index if re.fullmatch(r"r\d+", c)]
    if not rule_cols: return 0.0
    s = 0.0
    for c in rule_cols:
        try:
            v = row[c]
            if pd.isna(v): continue
            s += float(v)
        except: pass
    return s

# -------------------- selected inference --------------------

SEL_FLAG_CANDIDATES = ["selected","is_selected","chosen","pick"]
META_ID_CANDIDATES  = [
    "selected_option_id","final_option_id","picked_option_id",
    "chosen_option_id","meta_choice_id","meta_selected_id"
]
META_LBL_CANDIDATES = ["selected_option_label","final_option_label","pick_label","chosen_label"]

def infer_selected_mask(trace, meta):
    """
    Return a boolean Series (index aligned to 'trace') indicating the selected row.
    Strategy:
      1) use explicit selected flag if present
      2) join with meta on (condition, round, option_id) or (condition, round, option_label)
      3) fallback by rank==1 or max(score) within each (condition, round)
    """
    t = trace.copy()

    # 1) Explicit selected flag
    for c in SEL_FLAG_CANDIDATES:
        if c in t.columns:
            print(f"[info] Using explicit selected flag: {c}")
            return t[c].astype(bool)

    # Standardize keys
    t, round_col_t = std_round_col(t)
    t, optid_col_t = std_option_id(t)
    t, lbl_col_t   = std_option_label(t)

    # Ensure condition
    if "condition" not in t.columns:
        t["condition"] = "combined_meta"

    # 2) From meta file
    if meta is not None:
        m = meta.copy()
        # condition
        if "condition" not in m.columns:
            m["condition"] = "combined_meta"
        # round
        m, round_col_m = std_round_col(m)

        # (a) option_id based
        for c in META_ID_CANDIDATES:
            if c in m.columns and optid_col_t:
                print(f"[info] Inferring selection via meta ID column: {c}")
                m2 = m[[ "condition", round_col_m, c ]].copy()
                m2[c] = m2[c].astype(str)
                # left join to mark selected
                key_t = ["condition", round_col_t, optid_col_t]
                key_m = ["condition", round_col_m, c]
                j = t.merge(m2.rename(columns={c: "__sel_id__"}), left_on=key_t, right_on=key_m, how="left")
                selected = j["__sel_id__"].notna()
                return selected

        # (b) option_label based
        for c in META_LBL_CANDIDATES:
            if c in m.columns and lbl_col_t:
                print(f"[info] Inferring selection via meta LABEL column: {c}")
                m2 = m[[ "condition", round_col_m, c ]].copy()
                m2[c] = m2[c].astype(str).str.upper().str.strip()
                tt = t.copy()
                tt[lbl_col_t] = tt[lbl_col_t].astype(str).str.upper().str.strip()
                key_t = ["condition", round_col_t, lbl_col_t]
                key_m = ["condition", round_col_m, c]
                j = tt.merge(m2.rename(columns={c: "__sel_lbl__"}), left_on=key_t, right_on=key_m, how="left")
                selected = j["__sel_lbl__"].notna()
                return selected

    # 3) Fallback by rank==1 or max(score)
    if "rank" in t.columns and pd.api.types.is_numeric_dtype(pd.to_numeric(t["rank"], errors="coerce")):
        print("[info] Inferring selection via rank==1 per (condition, round)")
        t["rank"] = pd.to_numeric(t["rank"], errors="coerce")
        idx = t.sort_values(["condition", round_col_t, "rank"], na_position="last") \
               .groupby(["condition", round_col_t], dropna=False).head(1).index
        return t.index.isin(idx)

    if "score" in t.columns:
        print("[info] Inferring selection via max(score) per (condition, round)")
        t["score"] = pd.to_numeric(t["score"], errors="coerce")
        idx = t.sort_values(["condition", round_col_t, "score"], ascending=[True, True, False]) \
               .groupby(["condition", round_col_t], dropna=False).head(1).index
        return t.index.isin(idx)

    raise ValueError("Cannot infer selected rows: no explicit flag, no usable meta columns, no rank/score to fallback.")

# -------------------- metrics --------------------

def compute_violation_stats(trace_df, meta_df=None):
    t = trace_df.copy()

    # selected mask (robust inference)
    sel_mask = infer_selected_mask(t, meta_df)
    t["__selected__"] = sel_mask

    # kantian status & severity
    if "kantian_status" not in t.columns:
        for c in ["kantian","deontic_status","kantian_clean"]:
            if c in t.columns: t["kantian_status"]=t[c]; break
    if "violations" not in t.columns: t["violations"]=""

    t["sev_str"]   = t["violations"].apply(sev_from_violations)
    t["sev_flags"] = t.apply(sev_from_rule_flags, axis=1)
    t["kantian_severity"] = t["sev_str"].fillna(0) + t["sev_flags"].fillna(0)

    if "kantian_status" not in t.columns:
        t["kantian_status"] = np.where(t["kantian_severity"]==0, "clean", "violation")

    sel = t[t["__selected__"]].copy()
    if sel.empty:
        raise ValueError("Selected set is empty after inference.")

    if "condition" not in sel.columns:
        sel["condition"] = "combined_meta"

    def _agg(g):
        n=len(g); clean_n=(g["kantian_status"].astype(str).str.lower()=="clean").sum()
        return pd.Series({
            "Violation-free (%)": 100.0*clean_n/n if n else np.nan,
            "Mean Kantian severity": float(g["kantian_severity"].mean()) if n else np.nan,
            "n_selected": n
        })

    return sel.groupby("condition", dropna=False).apply(_agg).reset_index()

def compute_delta_and_conflict(meta_df, util_df=None):
    df = meta_df.copy()
    if "condition" not in df.columns:
        df["condition"]="combined_meta"
    if "switched" in df.columns:
        df["switched"]=df["switched"].astype(str).str.lower().isin(["1","true","yes","y"])
    for c in ["regret","util_best_score","delta_vs_price"]:
        if c in df.columns: df[c]=pd.to_numeric(df[c], errors="coerce")

    # Δ welfare vs price
    if "delta_vs_price" in df.columns and df["delta_vs_price"].notna().any():
        delta = df.groupby("condition")["delta_vs_price"].mean().rename("Δ welfare vs. price").reset_index()
    elif "regret" in df.columns and df["regret"].notna().any():
        delta = (-df["regret"]).groupby(df["condition"]).mean().rename("Δ welfare vs. price").reset_index()
    elif util_df is not None and {"condition","round_id","option_id","util_score","price_cad_per_cup","selected"}.issubset(util_df.columns):
        u=util_df.copy(); u["selected"]=u["selected"].astype(bool); sel=u[u["selected"]]
        idx=u.groupby(["condition","round_id"])["price_cad_per_cup"].idxmin()
        price_min=u.loc[idx,["condition","round_id","util_score"]].rename(columns={"util_score":"util_price_min"})
        j=sel.merge(price_min,on=["condition","round_id"],how="left")
        j["delta_vs_price_est"]=j["util_score"]-j["util_price_min"]
        delta=j.groupby("condition")["delta_vs_price_est"].mean().rename("Δ welfare vs. price").reset_index()
    else:
        raise ValueError("Cannot compute Δ welfare vs. price (no delta_vs_price, no regret, no utilitarian fallback).")

    # Conflict resolved (%)
    if "switched" in df.columns:
        conflict = df.groupby("condition")["switched"].mean().mul(100.0).rename("Conflict resolved (%)").reset_index()
    else:
        conflict = pd.DataFrame({"condition": df["condition"].dropna().unique(),
                                 "Conflict resolved (%)": np.nan})

    return delta.merge(conflict, on="condition", how="outer")

# -------------------- main --------------------

def main():
    meta  = ensure_condition(smart_read_one(["decision_meta_config*.csv","decision_meta*.csv"]), "combined_meta")
    if meta is None: raise FileNotFoundError("decision_meta_config*.csv not found in outputs/")
    trace = ensure_condition(smart_read_one(["policy_trace_text*.csv","decision_trace_config*.csv"]), "combined_meta")
    if trace is None: raise FileNotFoundError("policy_trace_text*.csv (or decision_trace_config*.csv) not found in outputs/")
    util  = ensure_condition(smart_read_one(["utilitarian_scores*.csv"]), "combined_meta")

    # compute parts
    kant  = compute_violation_stats(trace, meta_df=meta)
    dc    = compute_delta_and_conflict(meta, util_df=util)

    summary = kant.merge(dc, on="condition", how="outer")

    rename_map = {
        "no_expl":"No explanation","no-expl":"No explanation",
        "kantian_only":"Kantian-only","kantian-only":"Kantian-only",
        "util_only":"Utilitarian-only","util-only":"Utilitarian-only",
        "combined_meta":"Combined + Meta","combined+meta":"Combined + Meta","combined_meta_expl":"Combined + Meta",
    }
    summary["Condition"] = summary["condition"].replace(rename_map).fillna(summary["condition"])
    summary = summary.drop(columns=["condition"])

    fmt_pct = lambda x: np.nan if pd.isna(x) else round(float(x),1)
    fmt_f   = lambda x,nd=3: np.nan if pd.isna(x) else round(float(x),nd)
    summary["Δ welfare vs. price"]   = summary["Δ welfare vs. price"].apply(lambda v: fmt_f(v,3))
    summary["Violation-free (%)"]    = summary["Violation-free (%)"].apply(fmt_pct)
    summary["Mean Kantian severity"] = summary["Mean Kantian severity"].apply(lambda v: fmt_f(v,2))
    summary["Conflict resolved (%)"] = summary["Conflict resolved (%)"].apply(fmt_pct)

    order=["No explanation","Kantian-only","Utilitarian-only","Combined + Meta"]
    summary["__ord"]=summary["Condition"].apply(lambda c: order.index(c) if c in order else 999)
    summary=summary.sort_values(["__ord","Condition"]).drop(columns="__ord")

    out_csv = OUT / "condition_table.csv"
    summary.to_csv(out_csv, index=False)
    print("\n[done] wrote:", out_csv)
    print(summary.to_string(index=False))

if __name__ == "__main__":
    main()
