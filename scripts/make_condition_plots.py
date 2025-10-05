from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt

root = Path(__file__).resolve().parents[1]
out = root / "outputs"

summary = pd.read_csv(out / "condition_summary.csv")
order = ["none", "kantian", "utilitarian", "combined_meta"]
summary["condition"] = pd.Categorical(summary["condition"], categories=order, ordered=True)
summary = summary.sort_values("condition")

plt.figure()
ax = summary.plot(kind="bar", x="condition", y="welfare_uplift_vs_price", legend=False, edgecolor="black")
ax.set_title("Welfare uplift vs price baseline")
ax.set_xlabel("Condition")
ax.set_ylabel("Δ utilitarian score")
ax.axhline(0, linewidth=1)
for p in ax.patches:
    ax.annotate(f"{p.get_height():+.3f}", (p.get_x()+p.get_width()/2, p.get_height()), ha="center", va="bottom", fontsize=9)
plt.tight_layout()
plt.savefig(out / "cond_welfare_uplift.png", dpi=300)
plt.savefig(out / "cond_welfare_uplift.svg")

plt.figure()
ax2 = summary.plot(kind="bar", x="condition", y="violation_free_share", legend=False, edgecolor="black")
ax2.set_title("Violation-free share")
ax2.set_xlabel("Condition")
ax2.set_ylabel("Share (0–1)")
ax2.set_ylim(0, 1)
for p in ax2.patches:
    y = p.get_height()
    ax2.annotate(f"{y*100:.0f}%", (p.get_x()+p.get_width()/2, y), ha="center", va="bottom", fontsize=9)
plt.tight_layout()
plt.savefig(out / "cond_violation_free_share.png", dpi=300)
plt.savefig(out / "cond_violation_free_share.svg")

conf = pd.read_csv(out / "condition_conflict_resolution.csv")
rate = float(conf.loc[0, "value"]) if not conf.empty else float("nan")
with open(out / "cond_conflict_resolution.txt", "w", encoding="utf-8") as f:
    f.write(f"conflict_resolution_rate: {rate:.3f}\n")

print("Saved:",
      out / "cond_welfare_uplift.png",
      out / "cond_violation_free_share.png",
      "and",
      out / "cond_conflict_resolution.txt")

