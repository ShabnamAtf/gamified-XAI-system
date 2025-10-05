# tools/build_coffee_scenarios.py
from pathlib import Path
import csv

OUT_DIR = Path("data/coffee")
OUT_FILE = OUT_DIR / "coffee_scenarios.csv"

COLUMNS = [
    "round_idx","round_name","option_label","option_id","category","product_name","description",
    "price_cad_per_cup","carbon_g_co2e_per_cup","water_l_per_cup",
    "transparency_score","farmer_income_share_pct","deforestation_risk","shade_cert","child_labor_risk",
    "certs","packaging_recyclability","packaging_type",
    "taste_score_sca","freshness_days_since_roast","brew_time_min",
    "decaf_process","vegan_cert"
]

ROWS = [
    (1,"Whole Bean","A","r1A","whole_bean",
     "Single-origin, shade-grown, direct-trade (250g)",
     "Specialty single-origin; shade-grown; direct-trade.", 
     0.85, 90, 0.45, 0.90, 32, 0.10, True, 0.05,
     "Fairtrade;Organic;Bird-Friendly", 1, "paper",
     86, 7, 4.0, "none", True),
    (1,"Whole Bean","B","r1B","whole_bean",
     "Mass-market supermarket blend (500g)",
     "Industrial blend; low transparency; cheap price.",
     0.45, 110, 0.50, 0.30, 12, 0.60, False, 0.25,
     "", 0, "plastic", 75, 90, 4.0, "none", True),
    (1,"Whole Bean","C","r1C","pods_compostable",
     "Compostable capsule/pod (pack of 10)",
     "Convenient compostable pods; mid transparency.",
     1.10, 105, 0.45, 0.60, 18, 0.40, False, 0.15,
     "Compostable", 1, "compostable", 80, 14, 0.5, "none", True),

    (2,"Convenience","A","r2A","instant",
     "Freeze-dried instant (Fairtrade)",
     "Fairtrade instant coffee in recyclable jar.",
     0.50, 80, 0.40, 0.80, 25, 0.30, True, 0.10,
     "Fairtrade", 1, "glass", 74, 120, 0.5, "none", True),
    (2,"Convenience","B","r2B","concentrate",
     "Cold brew concentrate (1L; ~8 cups)",
     "Ready-to-dilute concentrate; plastic bottle.",
     1.30, 120, 0.50, 0.50, 15, 0.50, False, 0.20,
     "", 1, "plastic", 78, 20, 0.2, "none", True),
    (2,"Convenience","C","r2C","pods_aluminum",
     "Recyclable aluminum pod",
     "Quick pod; take-back aluminum program.",
     1.00, 95, 0.45, 0.60, 18, 0.40, False, 0.15,
     "", 1, "aluminum", 82, 10, 0.5, "none", True),

    (3,"Decaf","A","r3A","decaf",
     "Swiss Water decaf",
     "Chemical-free water process; shade-grown.",
     0.90, 95, 0.55, 0.85, 28, 0.30, True, 0.10,
     "Fairtrade;Organic", 1, "paper", 84, 12, 4.0, "water", True),
    (3,"Decaf","B","r3B","decaf",
     "Solvent-based decaf (no methylene chloride)",
     "Food-grade solvent; compliant supplier.",
     0.70, 100, 0.50, 0.60, 20, 0.50, False, 0.15,
     "", 1, "plastic", 80, 30, 4.0, "solvent_safe", True),
    (3,"Decaf","C","r3C","decaf",
     "Cheap industrial decaf (controversial solvent)",
     "Low price; weak transparency; risky solvent.",
     0.40, 110, 0.50, 0.30, 12, 0.60, False, 0.30,
     "", 0, "plastic-film", 72, 60, 4.0, "solvent_risky", True),

    (4,"RTD + Plant Milk","A","r4A","rtd",
     "Canned cold brew + vegan oat milk",
     "Aluminum can; barista-oat blend.",
     2.20, 160, 0.30, 0.60, 20, 0.40, False, 0.15,
     "", 1, "aluminum", 78, 15, 0.0, "none", True),
    (4,"RTD + Plant Milk","B","r4B","rtd",
     "Bottled RTD coffee + soy milk",
     "PET bottle; soy-based latte.",
     2.00, 150, 0.25, 0.60, 20, 0.40, False, 0.15,
     "", 1, "plastic", 77, 20, 0.0, "none", True),
    (4,"RTD + Plant Milk","C","r4C","powder_mix",
     "Vegan cappuccino powder (coffee + oat)",
     "Compostable sachets; add hot water.",
     0.80, 130, 0.35, 0.50, 18, 0.50, False, 0.20,
     "Compostable", 1, "compostable", 75, 60, 1.0, "none", True),

    (5,"Mocha / Cocoa","A","r5A","mocha",
     "Mocha with Fairtrade cocoa powder",
     "Ethical cocoa; audited supply chain.",
     1.10, 130, 0.35, 0.80, 26, 0.30, True, 0.10,
     "Fairtrade;Rainforest", 1, "paper", 83, 30, 3.0, "none", True),
    (5,"Mocha / Cocoa","B","r5B","mocha",
     "Mocha with high child-labor-risk cocoa",
     "Unverified cocoa; opaque sourcing.",
     0.90, 125, 0.35, 0.30, 15, 0.60, False, 0.60,
     "", 0, "plastic", 81, 30, 3.0, "none", True),
    (5,"Mocha / Cocoa","C","r5C","mocha",
     "Mocha sachet with compostable packaging",
     "Convenient sachets; mid transparency.",
     1.00, 120, 0.35, 0.60, 20, 0.40, False, 0.15,
     "Compostable", 1, "compostable", 80, 60, 1.0, "none", True),

    (6,"Taste vs Impact","A","r6A","whole_bean",
     "Specialty coffee, high cupping score (fresh roast)",
     "Fresh roast; direct-trade; shade-grown.",
     1.00, 100, 0.45, 0.85, 30, 0.20, True, 0.05,
     "Fairtrade;Bird-Friendly", 1, "paper", 88, 5, 4.0, "none", True),
    (6,"Taste vs Impact","B","r6B","whole_bean",
     "Mid-range blend with older roast date",
     "Budget blend; older stock; mixed sourcing.",
     0.60, 105, 0.45, 0.50, 20, 0.45, False, 0.20,
     "", 0, "plastic", 78, 90, 4.0, "none", True),
    (6,"Taste vs Impact","C","r6C","capsule",
     "Capsule with high freshness but more waste",
     "Very fresh; convenience; non-recyclable plastic.",
     1.20, 110, 0.45, 0.60, 18, 0.40, False, 0.15,
     "", 0, "plastic", 84, 7, 0.5, "none", True),
]

def main():
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    with OUT_FILE.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(COLUMNS)
        writer.writerows(ROWS)
    print(f"Wrote {len(ROWS)} rows to {OUT_FILE.resolve()}")

if __name__ == "__main__":
    main()

