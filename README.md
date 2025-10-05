# Gamified XAI System

Dual-logic XAI for ethical consumer decisions: **Kantian rules** + **utilitarian MCDA** with a **regret-bounded** meta-explainer.

**[📄 Preprint (PDF)](docs/meta-explained-preprint.pdf)** · [⚡ Quickstart](#quickstart) · [✨ Features](#features)

## Quickstart
```bash
python -m venv .venv && .\.venv\Scripts\activate   # Windows
pip install -r requirements.txt
streamlit run app.py   # اگر فایل UI در ui/ است: streamlit run ui/app.py
## Features
- Kantian rule checks (deontic constraints)
- Utilitarian MCDA (weighted, signed, min–max normalized)
- Regret-bounded meta-explainer (conflict resolution)
- Streamlit UI with per-round “Why/Details”
- Audit logs and reproducible configs
