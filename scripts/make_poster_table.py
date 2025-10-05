from pathlib import Path
import pandas as pd
import math

root = Path(__file__).resolve().parents[1]
out = root / "outputs"

# read
summary = pd.read_csv(out / "condition_summary.csv")
conf = (out / "condition_conflict_resolution.csv")
rate = None
if conf.exists():
    dfc = pd.read_csv(conf)
    if not dfc.empty and "value" in dfc.columns and not math.isnan(dfc.loc[0, "value"]):
        rate = float(dfc.loc[0, "value"])

# labels & formatting
order = ["none", "kantian", "utilitarian", "combined_meta"]
label = {
    "none": "No explanation",
    "kantian": "Kantian-only",
    "utilitarian": "Utilitarian-only",
    "combined_meta": "Combined + Meta",
}
summary["condition"] = pd.Categorical(summary["condition"], categories=order, ordered=True)
summary = summary.sort_values("condition")

summary["? welfare vs price"] = summary["welfare_uplift_vs_price"].map(lambda x: f"{x:+.3f}")
summary["Mean util"] = summary["mean_util"].map(lambda x: f"{x:.3f}")
summary["Violation-free"] = (summary["violation_free_share"] * 100).round(0).astype(int).map(lambda x: f"{x}%")
summary["Mean Kantian severity"] = summary["mean_kantian_severity"].map(lambda x: f"{x:.3f}")
summary["Condition"] = summary["condition"].map(label)

summary["Conflict resolved"] = "—"
if rate is not None:
    summary.loc[summary["Condition"] == "Combined + Meta", "Conflict resolved"] = f"{rate*100:.1f}%"

table = summary[["Condition", "? welfare vs price", "Mean util", "Violation-free", "Mean Kantian severity", "Conflict resolved"]]

# write CSV
table.to_csv(out / "poster_table.csv", index=False)

# write Markdown
md_lines = [
    "| " + " | ".join(table.columns) + " |",
    "| " + " | ".join(["---"] * len(table.columns)) + " |",
]
for _, r in table.iterrows():
    md_lines.append("| " + " | ".join(str(v) for v in r.tolist()) + " |")
(out / "poster_table.md").write_text("\n".join(md_lines), encoding="utf-8")

# write LaTeX (booktabs)
tex = r"""\begin{table}[t]
\centering
\small
\begin{tabular}{lrrrrr}
\toprule
Condition & $\Delta$ welfare vs price & Mean util & Violation-free & Mean Kantian severity & Conflict resolved \\
\midrule
"""
for _, r in table.iterrows():
    tex += f"{r['Condition']} & {r['? welfare vs price']} & {r['Mean util']} & {r['Violation-free']} & {r['Mean Kantian severity']} & {r['Conflict resolved']} \\\\\n"
tex += r"""\bottomrule
\end{tabular}
\caption{Condition-level outcomes across eight synthetic COFFEE scenarios.}
\label{tab:cond_summary}
\end{table}
"""
(out / "poster_table.tex").write_text(tex, encoding="utf-8")

print("Wrote:", out / "poster_table.csv", out / "poster_table.md", out / "poster_table.tex")



