# ui/blocks/condition_table.py
from pathlib import Path
import pandas as pd
import streamlit as st

def render_condition_table():
    ROOT = Path(__file__).resolve().parents[2]  # .../Logic
    out_csv = ROOT / "outputs" / "condition_table.csv"

    st.markdown("### Condition summary")
    if not out_csv.exists():
        st.info("condition_table.csv پیدا نشد. یک‌بار اسکریپت `scripts/build_condition_table.py` را اجرا کن.")
        return

    df = pd.read_csv(out_csv)
    st.dataframe(df, width='stretch')  # به‌جای use_container_width
    st.download_button(
        "Download condition_table.csv",
        data=df.to_csv(index=False).encode("utf-8"),
        file_name="condition_table.csv",
        mime="text/csv"
    )
