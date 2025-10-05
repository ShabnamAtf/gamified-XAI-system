import streamlit as st
import numpy as np
import pandas as pd


# ---------- Page setup ----------
st.set_page_config(page_title="Gamified XAI — Ethical Shopping (COFFEE)", layout="wide")
st.title("Gamified XAI — Ethical Shopping (COFFEE)")
st.caption("Budget allocation with explainable Kantian & utilitarian feedback.")

# ---------- Session state ----------
if "budget0" not in st.session_state:
    st.session_state.budget0 = 25.00
if "budget" not in st.session_state:
    st.session_state.budget = 25.00
if "round" not in st.session_state:
    st.session_state.round = 1

# ---------- Sidebar: Value tuning ----------
st.sidebar.header("Value tuning (weights & thresholds)")

# Weights for utilitarian MCDA (higher = more important)
w_price  = st.sidebar.slider("Weight: Price (? better)", 0.0, 2.0, 1.0, 0.1)
w_carbon = st.sidebar.slider("Weight: Carbon (? better)", 0.0, 2.0, 1.0, 0.1)
w_aw     = st.sidebar.slider("Weight: Animal welfare",    0.0, 2.0, 1.0, 0.1)
w_tr     = st.sidebar.slider("Weight: Transparency",      0.0, 2.0, 1.0, 0.1)
w_rec    = st.sidebar.slider("Weight: Recyclability",     0.0, 2.0, 1.0, 0.1)
w_lab    = st.sidebar.slider("Weight: Labour/Community",  0.0, 2.0, 1.0, 0.1)
delta    = st.sidebar.slider("Regret bound d",            0.00, 1.00, 0.20, 0.01)

st.sidebar.markdown("---")
st.sidebar.subheader("Kantian thresholds")
t_price  = st.sidebar.number_input("Max price (CAD)", min_value=0.0, value=6.00, step=0.1)
t_carbon = st.sidebar.number_input("Max carbon (kgCO2/kg)", min_value=0.0, value=1.50, step=0.1)
t_aw     = st.sidebar.number_input("Min animal welfare (0–100)", min_value=0, max_value=100, value=65, step=1)
t_tr     = st.sidebar.number_input("Min transparency (0–100)",    min_value=0, max_value=100, value=60, step=1)
t_rec    = st.sidebar.number_input("Min recyclability (%)",       min_value=0, max_value=100, value=70, step=1)
t_lab    = st.sidebar.number_input("Min labour/community (0–100)", min_value=0, max_value=100, value=60, step=1)

# ---------- Scenario (you can replace with your CSV) ----------
options = [
    {"name": "Local Co-op 1L",      "price": 4.79, "carbon": 0.9, "animal_welfare": 82, "transparency": 78, "recyclability": 90, "labour": 85},
    {"name": "Budget Industrial 1L","price": 3.49, "carbon": 1.6, "animal_welfare": 55, "transparency": 52, "recyclability": 65, "labour": 58},
    {"name": "Organic Import 1L",   "price": 5.39, "carbon": 1.2, "animal_welfare": 90, "transparency": 74, "recyclability": 88, "labour": 72},
]

df = pd.DataFrame(options)

# ---------- Helpers ----------
def minmax(series, higher_is_better=True):
    a = series.astype(float).values
    if np.ptp(a) == 0:
        z = np.full_like(a, 0.5, dtype=float)
    else:
        z = (a - a.min()) / (a.max() - a.min())
    return z if higher_is_better else 1 - z

def kantian_severity(row):
    viols = []
    # For positive-coded attributes: row < threshold ? violation
    if row.price > t_price:          viols.append(("Price", f"{row.price:.2f} > {t_price:.2f}"))
    if row.carbon > t_carbon:        viols.append(("Carbon", f"{row.carbon:.2f} > {t_carbon:.2f}"))
    if row.animal_welfare < t_aw:    viols.append(("Animal welfare", f"{row.animal_welfare:.0f} < {t_aw}"))
    if row.transparency   < t_tr:    viols.append(("Transparency", f"{row.transparency:.0f} < {t_tr}"))
    if row.recyclability  < t_rec:   viols.append(("Recyclability", f"{row.recyclability:.0f} < {t_rec}"))
    if row.labour         < t_lab:   viols.append(("Labour/Community", f"{row.labour:.0f} < {t_lab}"))
    return len(viols), viols

def util_score(df):
    # Normalize inside the scenario (price and carbon: lower is better)
    n_price  = minmax(df["price"],  higher_is_better=False)
    n_carbon = minmax(df["carbon"], higher_is_better=False)
    n_aw     = minmax(df["animal_welfare"], True)
    n_tr     = minmax(df["transparency"],   True)
    n_rec    = minmax(df["recyclability"],  True)
    n_lab    = minmax(df["labour"],         True)
    W = np.array([w_price, w_carbon, w_aw, w_tr, w_rec, w_lab])
    N = np.vstack([n_price, n_carbon, n_aw, n_tr, n_rec, n_lab]).T  # shape: (n_options, 6)
    scores = (N * W).sum(axis=1) / (W.sum() if W.sum() > 0 else 1.0)
    return scores

# Attach util score and Kantian severity
df["util"] = util_score(df)
sev_and_details = df.apply(kantian_severity, axis=1)
df["severity"] = [s for s, _ in sev_and_details]
df["viol_details"] = [d for _, d in sev_and_details]

# For Why? text
def why_text(row):
    lines = []
    lines.append(f"**Utilitarian (MCDA) score:** {row.util:.3f}")
    lines.append("• Price/Carbon ????????? ? ??????? + ????????? ???? (AW/Transparency/Recyclability/Labour).")
    if row.severity == 0:
        lines.append("**Kantian:** Violation-free ?")
    else:
        lines.append(f"**Kantian:** {row.severity} violation(s) ??")
        for k, v in row.viol_details:
            lines.append(f" – {k}: {v}")
    return "<br>".join(lines)

df["why_html"] = df.apply(why_text, axis=1)

# Determine recommended options
idx_util_best = int(df["util"].idxmax())
clean_mask = df["severity"] == 0
idx_best_clean = int(df[clean_mask]["util"].idxmax()) if clean_mask.any() else None

# ---------- Header row: round + budget ----------
left, right = st.columns([3, 1])
with left:
    st.subheader(f"Round {st.session_state.round} / 6 — Scenario S1")
    st.write("Choose one product (price must fit remaining budget). Click a policy to auto-pick and see explanations.")
with right:
    st.metric("Budget remaining (CAD)", f"{st.session_state.budget:.2f}")
    st.progress(min(1.0, st.session_state.budget / st.session_state.budget0))

# ---------- Conflict meter (only if util-best violates and a clean option exists) ----------
if df.loc[idx_util_best, "severity"] > 0 and idx_best_clean is not None:
    regret = df.loc[idx_util_best, "util"] - df.loc[idx_best_clean, "util"]
    st.write("**Conflict meter** (Util-best violates a rule):")
    frac = 0.0 if delta == 0 else max(0.0, min(1.0, 1.0 - regret/delta))
    st.progress(frac)
    st.caption(f"?Utility = {regret:.3f} | d = {delta:.3f} ? "
               f"{'Switch recommended' if regret <= delta else 'Keep util-best'}")

# ---------- Render product cards ----------
cols = st.columns(3, gap="large")
def render_card(col, row, idx):
    with col:
        st.markdown(f"### {row.name}")
        st.write(f"**Price:** ${row.price:.2f}")
        st.write(f"**Util score:** {row.util:+.3f}")
        sev_color = "#22c55e" if row.severity == 0 else "#ef4444"
        sev_text = f"<b>Kantian severity:</b> <span style='color:{sev_color}'>{row.severity}</span> {'??' if row.severity>0 else '?'}"
        st.markdown(sev_text, unsafe_allow_html=True)
        with st.expander("Why?"):
            st.markdown(row.why_html, unsafe_allow_html=True)
        if st.button("Choose", key=f"choose_{idx}"):
            if row.price <= st.session_state.budget:
                st.session_state.budget -= row.price
                st.session_state.round += 1
                st.success(f"Selected: {row.name} — budget now ${st.session_state.budget:.2f}")
            else:
                st.warning("Not enough budget for this option.")

for i, r in df.iterrows():
    render_card(cols[i], r, i)

# ---------- Auto-pick policies ----------
st.markdown("---")
c1, c2, c3, c4 = st.columns(4)

def choose_idx(idx, note=None):
    row = df.loc[idx]
    if row.price <= st.session_state.budget:
        st.session_state.budget -= row.price
        st.session_state.round += 1
        st.success(f"Selected: {row.name} — budget now ${st.session_state.budget:.2f}")
        if note:
            st.info(note)
    else:
        st.warning("Not enough budget for this option.")

with c1:
    if st.button("Auto-pick: No explanation"):
        # same as utilitarian choice without messaging
        choose_idx(idx_util_best)

with c2:
    if st.button("Auto-pick: Kantian"):
        # pick a violation-free option if possible, else min severity
        if clean_mask.any():
            idx = int(df[clean_mask]["util"].idxmax())
        else:
            idx = int(df["severity"].idxmin())
        choose_idx(idx)

with c3:
    if st.button("Auto-pick: Utilitarian"):
        choose_idx(idx_util_best)

with c4:
    if st.button("Auto-pick: Combined + Meta"):
        if df.loc[idx_util_best, "severity"] == 0:
            choose_idx(idx_util_best)  # already clean
        else:
            if idx_best_clean is not None:
                regret = df.loc[idx_util_best, "util"] - df.loc[idx_best_clean, "util"]
                if regret <= delta:
                    choose_idx(idx_best_clean, note=f"Switched to clean option (?Utility = {regret:.3f} = d)")
                else:
                    choose_idx(idx_util_best, note=f"Stayed with util-best (?Utility = {regret:.3f} > d)")
            else:
                choose_idx(idx_util_best, note="No violation-free option available")

st.caption("Build check: 2025-09-01 22:05")
# ??????? ???? ??????? ?? ???? ????:
st.sidebar.write("Script path:", __file__)
st.markdown("---")
st.caption("Tip: ??? ?? ??? ???. ?????? ? ???????? ?? ????????? ?? CSV/YAML ?????? ??????? ???? ? ?????? ?? ?? ??????? ????? ????.")



