# Gamified XAI System

Dual-logic XAI for ethical consumer decisions: **Kantian rules** for deontic constraints + **utilitarian MCDA** for welfare, coordinated by a **regret-bounded meta-explainer**. Includes configs, Streamlit UI, synthetic coffee scenarios, and audit-ready logs.

## Quickstart
```bash
python -m venv .venv && . .venv/Scripts/activate   # Windows
# or: python3 -m venv .venv && source .venv/bin/activate  # macOS/Linux
pip install -r requirements.txt
streamlit run app.py
[📄 Preprint (PDF)](docs/meta-explained-preprint.pdf)
