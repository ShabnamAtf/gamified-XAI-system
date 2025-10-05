from pathlib import Path
import pandas as pd
root = Path(__file__).resolve().parents[1]
p = root/"outputs/play_log.csv"
if not p.exists(): raise SystemExit("Missing outputs/play_log.csv (finish a 6-round session in the UI).")
df = pd.read_csv(p)
summary = {"mean_util": round(df["util_score"].mean(),3),
           "mean_kantian_severity": round(df["kantian_severity_sum"].mean(),3),
           "violation_free_share": round((df["kantian_severity_sum"]==0).mean(),3)}
out = root/"outputs"; out.mkdir(exist_ok=True)
tab = pd.DataFrame([summary])
tab.to_csv(out/"session_summary.csv", index=False)
(out/"session_summary.md").write_text(tab.to_markdown(index=False))
print(tab.to_markdown(index=False))



