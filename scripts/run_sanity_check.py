from pathlib import Path
import argparse, json
import yaml
import pandas as pd

REQUIRED_COLS = [
    "scenario_id","product_id","product_name","category",
    "price_cad","carbon_kgco2_perkg","animal_welfare_0_100",
    "transparency_0_100","packaging_recyclability_pct","labour_community_0_100"
]

def ok(msg): print(f"[OK]   {msg}")
def warn(msg): print(f"[WARN] {msg}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--config", default="stimuli/experiment_config.yml")
    args = ap.parse_args()

    root = Path(__file__).resolve().parents[1]
    cfg_path = (root / args.config).resolve()
    with open(cfg_path, "r", encoding="utf-8") as f:
        cfg = yaml.safe_load(f)

    def p(key):
        return (root / cfg["paths"][key]).resolve()

    scen = pd.read_csv(p("scenarios"))
    with open(p("attribute_schema"), "r", encoding="utf-8") as f: schema = json.load(f)
    with open(p("kantian_rules"), "r", encoding="utf-8") as f: rules = yaml.safe_load(f)
    with open(p("utilitarian_weights"), "r", encoding="utf-8") as f: weights = yaml.safe_load(f)
    templ = pd.read_csv(p("explanation_templates"))

    print("=== Sanity Check ===")

    missing = [c for c in REQUIRED_COLS if c not in scen.columns]
    if missing:
        raise SystemExit(f"[ERROR] Missing columns in scenarios.csv: {missing}")
    ok("scenarios.csv columns present")

    counts = scen.groupby("scenario_id").size()
    if not ((counts >= 3) & (counts <= 4)).all():
        raise SystemExit(f"[ERROR] Each scenario should have 3–4 options. Got:\n{counts}")
    ok(f"{len(counts)} scenarios found; options per scenario OK")

    conds = [c["id"] if isinstance(c, dict) else c for c in cfg["conditions"]]
    if set(conds) != {"none","kantian","utilitarian","combined_meta"}:
        warn(f"Unexpected conditions: {conds}")
    else:
        ok("conditions match expected set")

    attr_cfg = schema["attributes"]
    out_of_range = []
    for col, meta in attr_cfg.items():
        if col not in scen.columns: continue
        lo, hi = meta["raw_range"]
        vmin, vmax = scen[col].min(), scen[col].max()
        if vmin < lo - 1e-9 or vmax > hi + 1e-9:
            out_of_range.append((col, (vmin, vmax), (lo, hi)))
    if out_of_range:
        warn("some attributes fall outside declared raw_range:")
        for col, (vmin, vmax), (lo, hi) in out_of_range:
            warn(f"  - {col}: data [{vmin}, {vmax}] vs schema [{lo}, {hi}]")
    else:
        ok("attribute values within declared raw_range")

    if "rules" not in rules or not rules["rules"]:
        raise SystemExit("[ERROR] kantian_rules.yml has no 'rules'")
    ok(f"kantian_rules: {len(rules['rules'])} rules loaded")

    w = weights.get("weights", {})
    miss_w = [c for c in ["price_cad","carbon_kgco2_perkg","animal_welfare_0_100",
                          "transparency_0_100","packaging_recyclability_pct","labour_community_0_100"]
              if c not in w]
    if miss_w:
        raise SystemExit(f"[ERROR] utilitarian_weights.yml missing weights for: {miss_w}")
    ok("utilitarian weights present")

    if templ.empty or not {"condition","rule_id","template_text"}.issubset(templ.columns):
        raise SystemExit("[ERROR] explanation_templates.csv missing required columns")
    ok(f"explanation templates: {len(templ)} rows")

    print("\nALL CHECKS PASSED ✅")

if __name__ == "__main__":
    main()

