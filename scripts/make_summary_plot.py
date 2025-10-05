from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt

root = Path(__file__).resolve().parents[1]
out_dir = root / "outputs"
df = pd.read_csv(out_dir / "options_scored.csv")


df["util_score_01"] = df.groupby("scenario_id")["util_score"]\
    .transform(lambda s: (s - s.min()) / (s.max() - s.min() + 1e-12))


per_s = (
    df.assign(vfree=(df["kantian_severity_sum"]==0).astype(int))
      .groupby("scenario_id")["vfree"].mean()
      .sort_index()
)

plt.figure()
ax = per_s.plot(kind="bar", edgecolor="black")
ax.set_title("Violation-Free Options per Scenario")
ax.set_xlabel("Scenario")
ax.set_ylabel("Share violation-free (%)")
ax.set_ylim(0, 1)

for p in ax.patches:
    y = p.get_height()
    ax.annotate(f"{y*100:.0f}%", (p.get_x() + p.get_width()/2, y),
                ha="center", va="bottom", fontsize=9)
plt.tight_layout()
plt.savefig(out_dir / "fig_violation_free_per_scenario.svg")
plt.savefig(out_dir / "fig_violation_free_per_scenario.png", dpi=300)


plt.figure()
df["util_score_01"].plot(kind="hist", bins=10, edgecolor="black")
plt.title("Distribution of Utilitarian Scores (0–1 within scenario)")
plt.xlabel("Utilitarian score (0–1, within scenario)")
plt.ylabel("Count")
plt.tight_layout()
plt.savefig(out_dir / "fig_util_score_hist_norm01.svg")
plt.savefig(out_dir / "fig_util_score_hist_norm01.png", dpi=300)

print("Saved figures to:", out_dir)

