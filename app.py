import streamlit as st
from pathlib import Path
import pandas as pd
from scripts.engine import load_resources, prepare_scene, decide, explain_kantian, explain_utilitarian

ROOT = Path(__file__).resolve().parent
st.set_page_config(page_title="Gamified XAI — Ethical Shopping", page_icon="??", layout="wide")

scen_all, schema, rules, uw, cfg, templates = load_resources()
budget_total = float(cfg.get("budget_cad", 25))
round_ids = list(scen_all["scenario_id"].unique())
rounds = int(cfg.get("rounds", min(6, len(round_ids))))
round_ids = round_ids[:rounds]

if "state" not in st.session_state:
    st.session_state.state = {
        "i": 0,
        "remaining": budget_total,
        "log": []
    }

st.title("Gamified XAI — Ethical Shopping (COFFEE)")
st.caption("Budget allocation with explainable Kantian & utilitarian feedback")

colA, colB = st.columns([3,1])
with colB:
    st.metric("Budget remaining (CAD)", f"{st.session_state.state['remaining']:.2f}")
    st.progress(max(0.0, min(1.0, st.session_state.state["remaining"]/budget_total)))

with colA:
    if st.session_state.state["i"] >= rounds:
        st.success("Session complete.")
        log = pd.DataFrame(st.session_state.state["log"])
        out = ROOT/"outputs"/"play_log.csv"
        out.parent.mkdir(exist_ok=True)
        log.to_csv(out, index=False)
        st.download_button("Download session log (CSV)", data=out.read_bytes(), file_name="play_log.csv")
        st.stop()

    sid = round_ids[st.session_state.state["i"]]
    st.subheader(f"Round {st.session_state.state['i']+1} / {rounds} — Scenario {sid}")
    df = prepare_scene(scen_all, schema, rules, uw, sid)

    st.write("Choose one product (price must fit remaining budget). Click a policy to auto-pick and see explanations.")
    cols = st.columns(len(df))
    picks = {}
    for c,(idx,row) in zip(cols, df.iterrows()):
        with c:
            st.markdown(f"**{row['product_name']}**")
            st.write(f"Price: ${row['price_cad']:.2f}")
            st.write(f"Util score: {row['util_score']:+.3f}")
            st.write(f"Kantian severity: {int(row['kantian_severity_sum'])}")
            can_afford = row["price_cad"] <= st.session_state.state["remaining"]
            disabled = not can_afford
            if st.button("Choose", key=f"choose_{idx}", disabled=disabled):
                st.session_state.state["selected"] = int(idx)

    st.divider()
    pol1, pol2, pol3, pol4 = st.columns(4)
    picks = {}
    with pol1:
        if st.button("Auto-pick: No explanation"):
            r = decide(df, "none")
            st.session_state.state["selected"] = int(r.name)
    with pol2:
        if st.button("Auto-pick: Kantian"):
            r = decide(df, "kantian")
            st.session_state.state["selected"] = int(r.name)
    with pol3:
        if st.button("Auto-pick: Utilitarian"):
            r = decide(df, "utilitarian")
            st.session_state.state["selected"] = int(r.name)
    with pol4:
        if st.button("Auto-pick: Combined + Meta"):
            r = decide(df, "combined_meta")
            st.session_state.state["selected"] = int(r.name)

    st.divider()
    sel_idx = st.session_state.state.get("selected", None)
    if sel_idx is not None and sel_idx in df.index:
        sel = df.loc[sel_idx]
        st.info(f"Selected: **{sel['product_name']}** — ${sel['price_cad']:.2f}")
        with st.expander("Kantian explanation"):
            st.write(explain_kantian(sel["kantian_msgs"], templates))
        with st.expander("Utilitarian explanation"):
            st.write(explain_utilitarian(sel, uw))
        with st.expander("Combined/meta rationale"):
            if sel["kantian_severity_sum"]==0:
                st.write("Chose a rule-compliant option within the welfare margin.")
            else:
                st.write("Stayed with the utilitarian best; no rule-compliant near-parity alternative available.")

        colN, colNext = st.columns([1,3])
        with colN:
            if st.button("Confirm & Next"):
                if sel["price_cad"] > st.session_state.state["remaining"]:
                    st.warning("Insufficient budget for this item.")
                else:
                    st.session_state.state["remaining"] -= float(sel["price_cad"])
                    st.session_state.state["log"].append({
                        "round": st.session_state.state["i"]+1,
                        "scenario_id": sel["scenario_id"],
                        "product_id": sel["product_id"],
                        "product_name": sel["product_name"],
                        "price_cad": float(sel["price_cad"]),
                        "util_score": float(sel["util_score"]),
                        "kantian_severity_sum": int(sel["kantian_severity_sum"])
                    })
                    st.session_state.state["i"] += 1
                    st.session_state.state.pop("selected", None)
                    st.rerun()
                    


