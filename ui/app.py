# ui/app.py
# -*- coding: utf-8 -*-

import streamlit as st
import pandas as pd
from pathlib import Path
import sys
import glob
from PIL import Image, ImageOps, ImageDraw
from datetime import datetime
import csv
import json
import subprocess
import re

# --- ensure we can import from ui/blocks (for render_condition_table) ---
APP_DIR = Path(__file__).resolve().parent          # .../Logic/ui
ROOT    = APP_DIR.parent                           # .../Logic
if str(APP_DIR) not in sys.path:
    sys.path.insert(0, str(APP_DIR))

# Local modules
from blocks.condition_table import render_condition_table
from meta_explainer import meta_explain, Candidate

# ---------- config & paths ----------
st.set_page_config(page_title="Ethical Coffee Choices", layout="wide")

SCENARIO_CSV = ROOT / "data" / "coffee" / "coffee_scenarios.csv"
TRACE_CSV    = ROOT / "outputs" / "policy_trace_text.csv"
LOG_PATH     = ROOT / "outputs" / "play_log.csv"
WEIGHTS_JSON = ROOT / "data" / "coffee" / "utilitarian_weights.json"
REPRO_BAT    = ROOT / "reproduce_all.bat"
BUILD_SCRIPT = ROOT / "scripts" / "build_condition_table.py"  # optional: for summary tab

# Unified card image size (center-crop to exact size)
FIT_SIZE = (1024, 640)

# ---------- helpers ----------
def clean_str(x):
    if x is None:
        return ""
    try:
        if isinstance(x, float) and pd.isna(x):
            return ""
    except Exception:
        pass
    s = str(x).strip()
    return "" if s.lower() in ("nan", "none", "null") else s

def letter_index(letter: str) -> int:
    return {"A": 1, "B": 2, "C": 3}.get(str(letter).upper(), 1)

def option_index(round_idx: int, opt_label: str) -> int:
    return (round_idx - 1) * 3 + letter_index(opt_label)   # 1..18

def find_image_path(round_idx: int, option_id: str, option_label: str):
    # 1) specific by option_id
    hits = glob.glob(str(APP_DIR / "images" / f"{option_id}.*"))
    if hits:
        return Path(hits[0])
    # 2) numbered coffee{1..18}
    idx = option_index(round_idx, option_label)
    hits = glob.glob(str(APP_DIR / "images" / f"coffee{idx}.*"))
    if hits:
        return Path(hits[0])
    # 3) fallback by round (coffee{round})
    hits = glob.glob(str(APP_DIR / "images" / f"coffee{round_idx}.*"))
    if hits:
        return Path(hits[0])
    return None

def load_card_image(round_idx: int, option_id: str, option_label: str) -> Image.Image:
    p = find_image_path(round_idx, option_id, option_label)
    img = None
    if p and p.exists():
        try:
            img = Image.open(p).convert("RGB")
        except Exception:
            img = None
    if img is None:
        ph = Image.new("RGB", FIT_SIZE, (243, 232, 219))
        d = ImageDraw.Draw(ph)
        d.text((20, 20), "No image", fill=(60, 47, 40))
        return ph
    return ImageOps.fit(img, FIT_SIZE, method=Image.Resampling.LANCZOS, centering=(0.5, 0.5))

def badge_html(row) -> str:
    chips = []
    try:
        chips.append(f"Price ${float(row.price_cad_per_cup):.2f}/cup")
    except Exception:
        pass
    try:
        chips.append(f"CO₂ {int(float(row.carbon_g_co2e_per_cup))} g")
    except Exception:
        pass
    try:
        if str(row.packaging_type):
            chips.append(f"Pack {row.packaging_type}")
    except Exception:
        pass
    return " ".join(f'<span class="badge">{c}</span>' for c in chips)

def violations_html(vio: str) -> str:
    s = (vio or "").replace(",", ";").strip()
    codes = [c.strip() for c in s.split(";") if c.strip()]
    if not codes:
        return '<div class="vio-wrap"><div class="vio-label">Violations:</div><span class="badge">none</span></div>'
    chips = "".join(f'<span class="vio-chip">{c}</span>' for c in codes)
    return f'<div class="vio-wrap"><div class="vio-label">Violations:</div><div class="vio-list">{chips}</div></div>'

def df_to_green_html(df: pd.DataFrame) -> str:
    df2 = df.copy()
    if "price_cad_per_cup" in df2:
        df2["price_cad_per_cup"] = pd.to_numeric(df2["price_cad_per_cup"], errors="coerce").map(
            lambda x: f"${x:.2f}" if pd.notna(x) else ""
        )
    if "carbon_g_co2e_per_cup" in df2:
        df2["carbon_g_co2e_per_cup"] = pd.to_numeric(df2["carbon_g_co2e_per_cup"], errors="coerce").map(
            lambda x: f"{int(x)} g" if pd.notna(x) else ""
        )
    cols = [c for c in ["round","option_id","name","price_cad_per_cup","carbon_g_co2e_per_cup"] if c in df2.columns]
    df2 = df2[cols]
    return df2.to_html(index=False, classes="green-table", border=0, escape=False)

# Parse utilitarian drivers string to a dict, e.g. "- child(0.20); + transp(0.00)"
def parse_util_drivers(s: str) -> dict:
    if not s:
        return {}
    pairs = re.findall(r'([+-])\s*([a-zA-Z_][\w\-]*)\s*\(\s*([-+]?\d*\.?\d+)\s*\)', s)
    out = {}
    for sign, key, val in pairs:
        v = float(val)
        if sign == "-":
            v = -v
        out[key] = v
    return out

# ---------- theme ----------
THEME_CSS = """
<style>
:root{
  --brown-700:#8d6e63; --brown-600:#a6826d; --brown-500:#b08968;
  --cream-50:#fbf7f0; --cream-100:#f7efe4; --cream-200:#efe4d6;
}
html, body, .stApp, div, p, span, label, h1,h2,h3,h4,h5,h6 {
  color: #1a1a1a !important;
  font-family: 'Inter', system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif;
}
.stApp { background: var(--cream-100); }
.stApp::before{
  content:""; position:fixed; inset:0; pointer-events:none; opacity:.14;
  background-image:url("https://www.transparenttextures.com/patterns/coffee-beans.png");
}
.topbar { position:sticky; top:0; z-index:50; padding:.65rem 1rem;
  background:linear-gradient(180deg, rgba(247,239,228,.98) 0%, rgba(247,239,228,.86) 100%);
  border-bottom:1px solid #e6d8c7; backdrop-filter:saturate(1.2) blur(6px);
}
.card { background:#fff7ed; border-radius:16px; padding:12px; margin:8px 4px;
  border:1px solid #eadfcc; box-shadow:0 6px 16px rgba(0,0,0,.10); transition:.15s all;
}
.card:hover { transform: translateY(-2px); box-shadow:0 10px 24px rgba(0,0,0,.16); }
.card.selected { border:2px solid var(--brown-600); box-shadow:0 12px 28px rgba(176,137,104,.22); }
.card-img-wrap { width:100%; overflow:hidden; border-radius:12px; margin-bottom:10px; background:#f3e8db; }
.card-img { width:100%; height:auto; display:block; }
.badge { display:inline-block; padding:3px 10px; margin-right:6px; margin-top:6px;
  border-radius:999px; background:#efe5d8; color:#3b2f28; font-size:12px; border:1px solid #e2d6c6;
}
.pick-btn button { width:100%; background:var(--brown-700); border:0; color:white; }
.pick-btn button:hover { background:var(--brown-600); }
.note { color:#3b2f28; font-style:italic; margin-top:.25rem; }
hr { border:0; border-top:1px solid #e6d8c7; margin:1.1rem 0; }
.vio-wrap { margin-top:.25rem; }
.vio-label { font-weight:600; margin-bottom:4px; }
.vio-list { display:flex; gap:6px; flex-wrap:wrap; align-items:center; }
.vio-chip { background:#1f2937; color:#34d399; padding:4px 10px; border-radius:10px; font-weight:600; font-size:12px; }
</style>
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap" rel="stylesheet">
"""
st.markdown(THEME_CSS, unsafe_allow_html=True)

GREEN_TABLE_CSS = """
<style>
.green-table{width:100%;border-collapse:collapse;background:#E7F2EE;color:#0F2E26;
  border:1px solid #B6D9CF;border-radius:12px;overflow:hidden;}
.green-table th{background:#11856B;color:#ffffff;text-align:left;padding:10px 12px;
  position:sticky;top:0;}
.green-table td{padding:10px 12px;border-bottom:1px solid #B6D9CF;}
.green-table tr:nth-child(even){background:#F3F9F7;}
.green-table tr:hover{background:#D6EFE7;}
</style>
"""
st.markdown(GREEN_TABLE_CSS, unsafe_allow_html=True)

# ---------- data ----------
if not SCENARIO_CSV.exists():
    st.error(f"Scenario file not found: {SCENARIO_CSV}")
    st.stop()

df = pd.read_csv(SCENARIO_CSV)
trace = pd.read_csv(TRACE_CSV) if TRACE_CSV.exists() else pd.DataFrame()

# Explanation index (from policy_trace_text.csv)
explain = {}
if not trace.empty:
    for r in trace.itertuples(index=False):
        score_val = getattr(r, "score", None)
        rank_val  = getattr(r, "rank", None)
        try:
            score_val = None if score_val is None or pd.isna(score_val) else float(score_val)
            rank_val  = None if rank_val  is None or pd.isna(rank_val)  else int(rank_val)
        except Exception:
            pass
        explain[(int(r.round_idx), str(r.option_id))] = {
            "drivers":        clean_str(getattr(r, "drivers", "")),
            "kantian_status": clean_str(getattr(r, "kantian_status", "")),
            "violations":     clean_str(getattr(r, "violations", "")),
            "score":          score_val,
            "rank":           rank_val,
            "decision_note":  clean_str(getattr(r, "decision_note", "")),
        }

def show_reasons(round_idx: int, option_id: str):
    info = explain.get((round_idx, option_id))
    if not info:
        st.info("No explanation found. Run the pipeline to generate policy_trace_text.csv.")
        return
    c1, c2 = st.columns(2)
    with c1:
        st.caption("Utilitarian drivers")
        st.write(info.get("drivers") or "-")
    with c2:
        st.caption("Kantian status")
        st.write(f"Status: **{info.get('kantian_status','-')}**")
        st.markdown(violations_html(info.get("violations")), unsafe_allow_html=True)
    if info.get("decision_note"):
        st.markdown(f'<div class="note">{info["decision_note"]}</div>', unsafe_allow_html=True)

# ---------- state ----------
if "picks" not in st.session_state:
    st.session_state.picks = {}

# ---------- header ----------
picked_count = len(st.session_state.picks)
st.markdown(f"""
<div class="topbar">
  <h1 style="margin:0;">☕ Ethical Coffee Choices</h1>
  <div>Pick one per round — photos, quick stats, and reasons included.</div>
  <div>Selected rounds: <b>{picked_count}/6</b></div>
</div>
""", unsafe_allow_html=True)
st.write("")

# ---------- tabs ----------
tab_compare, tab_summary, tab_settings = st.tabs(["Compare", "Summary", "Settings"])

# ---------- tab: Compare ----------
with tab_compare:
    if "round_idx" not in df.columns or "option_label" not in df.columns:
        st.error("The scenario CSV is missing required columns like 'round_idx' or 'option_label'.")
        st.stop()

    for rnd in sorted(pd.to_numeric(df["round_idx"], errors="coerce").dropna().astype(int).unique().tolist()):
        rows = df[df["round_idx"] == rnd].copy()
        round_name = rows["round_name"].iloc[0] if "round_name" in rows.columns else f"Round {rnd}"
        st.subheader(f"Round {rnd}: {round_name}")

        # Build candidates for meta-explainer and show its summary
        candidates = []
        for opt in rows.itertuples(index=False):
            opt_id    = str(opt.option_id)
            opt_label = str(opt.option_label)
            meta_info = explain.get((int(rnd), opt_id), {})
            util_dr   = parse_util_drivers(meta_info.get("drivers", ""))
            kant_stat_raw = meta_info.get("kantian_status", "").lower()
            kant_stat = "violations" if "violation" in kant_stat_raw else "ok"
            k_rules = [s.strip() for s in (meta_info.get("violations", "") or "").replace(",", ";").split(";") if s.strip()]
            score = meta_info.get("score") or 0.0
            candidates.append(Candidate(
                option_id=opt_id,
                name=f"{opt_label}: {getattr(opt, 'product_name', opt_id)}",
                mcda_score=float(score),
                util_drivers=util_dr,
                kantian_status=kant_stat,
                kantian_rules=k_rules
            ))
        try:
            meta = meta_explain(candidates)
            st.caption("Meta-explainer")
            st.write(meta.get("summary", ""))
        except Exception as e:
            st.warning(f"Meta-explainer failed: {e}")
            meta = {"recommended_id": None, "action": "none"}

        cols = st.columns(3)
        for i, opt_row in enumerate(rows.itertuples(index=False)):
            opt_id = str(opt_row.option_id)
            opt_label = str(opt_row.option_label)
            selected = (st.session_state.picks.get(rnd) == opt_id)
            is_meta_rec = meta.get("recommended_id") == opt_id and meta.get("action") != "block"

            with cols[i]:
                card_cls = "card selected" if selected else "card"
                st.markdown(f'<div class="{card_cls}">', unsafe_allow_html=True)

                if is_meta_rec:
                    st.markdown('<span class="badge">Meta: recommended</span>', unsafe_allow_html=True)

                img = load_card_image(int(rnd), opt_id, opt_label)
                st.markdown('<div class="card-img-wrap">', unsafe_allow_html=True)
                # No width param → avoids deprecation and lets CSS size it.
                st.image(img)
                st.markdown('</div>', unsafe_allow_html=True)

                st.markdown(f"### {opt_label}: {getattr(opt_row, 'product_name', opt_id)}")
                st.write(getattr(opt_row, "description", ""))

                # chips
                try:
                    st.markdown(badge_html(opt_row), unsafe_allow_html=True)
                except Exception:
                    pass

                c1, c2 = st.columns([1,1])
                with c1:
                    if st.button(f"Select {opt_label}", key=f"pick-{rnd}-{opt_id}"):
                        st.session_state.picks[rnd] = opt_id
                        meta2 = explain.get((int(rnd), opt_id), {})
                        drivers       = meta2.get("drivers")
                        util_score    = meta2.get("score")
                        kant_status   = meta2.get("kantian_status")
                        violations    = meta2.get("violations")
                        LOG_PATH.parent.mkdir(parents=True, exist_ok=True)
                        header = ["ts","round_idx","option_id","product_name","util_score","drivers","kantian_status","violations"]
                        csv_row = [
                            datetime.utcnow().isoformat(timespec="seconds"),
                            int(rnd),
                            opt_id,
                            getattr(opt_row, "product_name", opt_id),
                            util_score,
                            drivers,
                            kant_status,
                            violations,
                        ]
                        write_header = not LOG_PATH.exists()
                        with open(LOG_PATH, "a", newline="", encoding="utf-8") as f:
                            w = csv.writer(f)
                            if write_header:
                                w.writerow(header)
                            w.writerow(csv_row)
                        st.rerun()
                with c2:
                    with st.expander("Why / details"):
                        show_reasons(int(rnd), opt_id)

                st.markdown("</div>", unsafe_allow_html=True)

        st.markdown("<hr/>", unsafe_allow_html=True)

    # Picks summary (per-session)
    st.header("Your picks")
    if st.session_state.picks:
        summary_rows = []
        for rnd, oid in sorted(st.session_state.picks.items()):
            r = df[(df.round_idx==rnd) & (df.option_id.astype(str)==oid)]
            if r.empty:
                continue
            r = r.iloc[0]
            summary_rows.append({
                "round": rnd,
                "option_id": oid,
                "name": getattr(r, "product_name", oid),
                "price_cad_per_cup": getattr(r, "price_cad_per_cup", None),
                "carbon_g_co2e_per_cup": getattr(r, "carbon_g_co2e_per_cup", None),
            })
        if summary_rows:
            sel = pd.DataFrame(summary_rows)
            sel["price_cad_per_cup"] = pd.to_numeric(sel["price_cad_per_cup"], errors="coerce")
            sel["carbon_g_co2e_per_cup"] = pd.to_numeric(sel["carbon_g_co2e_per_cup"], errors="coerce")
            c1, c2 = st.columns([3,1])
            with c1:
                html = df_to_green_html(sel)
                st.markdown(html, unsafe_allow_html=True)
            with c2:
                st.metric("Total price / cup", f"${sel['price_cad_per_cup'].sum():.2f}")
                st.metric("Avg CO₂ / cup", f"{sel['carbon_g_co2e_per_cup'].mean():.0f} g")
        else:
            st.info("No valid picks yet.")
    else:
        st.info("No picks yet.")

# ---------- tab: Summary (Condition table from outputs) ----------
with tab_summary:
    st.subheader("Condition summary (from outputs/)")
    # Optional: build/refresh the summary CSV right here
    cols = st.columns(2)
    with cols[0]:
        if st.button("Rebuild condition_table.csv"):
            try:
                if not BUILD_SCRIPT.exists():
                    st.error(f"Build script not found: {BUILD_SCRIPT}")
                else:
                    # Call your venv python to run the build script
                    py = ROOT / ".venv" / "Scripts" / "python.exe"
                    cmd = [str(py if py.exists() else sys.executable), "-u", str(BUILD_SCRIPT)]
                    subprocess.run(cmd, check=True, cwd=str(ROOT))
                    st.success("Rebuilt successfully.")
                    st.rerun()
            except subprocess.CalledProcessError as e:
                st.error(f"Build failed: {e}")
            except Exception as e:
                st.error(f"Unexpected error: {e}")
    with cols[1]:
        st.caption(f"CSV path: {ROOT / 'outputs' / 'condition_table.csv'}")

    # Render the table (reads outputs/condition_table.csv)
    render_condition_table()

# ---------- tab: Settings ----------
with tab_settings:
    st.subheader("Utilitarian Weights")
    st.caption("Adjust a few key weights. Click “Apply weights & recompute” to refresh policy outputs.")

    w_price   = st.slider("Price weight", -1.0, 1.0, -0.25, 0.01)
    w_child   = st.slider("Child labor risk weight", -1.0, 1.0, -0.20, 0.01)
    w_transp  = st.slider("Transparency weight", -1.0, 1.0, 0.15, 0.01)
    w_defor   = st.slider("Deforestation risk weight", -1.0, 1.0, -0.10, 0.01)

    weights = {
        "price_cad_per_cup": w_price,
        "child_labor_risk":  w_child,
        "transparency_score": w_transp,
        "deforestation_risk": w_defor,
    }

    if st.button("Apply weights & recompute"):
        try:
            WEIGHTS_JSON.parent.mkdir(parents=True, exist_ok=True)
            with open(WEIGHTS_JSON, "w", encoding="utf-8") as f:
                json.dump({"weights": weights}, f, ensure_ascii=False, indent=2)
            st.info("Weights saved to data/coffee/utilitarian_weights.json")

            if REPRO_BAT.exists():
                subprocess.run(
                    [r"C:\Windows\System32\cmd.exe", "/c", str(REPRO_BAT)],
                    check=True, cwd=str(ROOT)
                )
                st.success("Recomputed successfully.")
            else:
                st.warning("reproduce_all.bat not found. Please create it to enable one-click recompute.")

            st.rerun()
        except Exception as e:
            st.error(f"Recompute failed: {e}")
