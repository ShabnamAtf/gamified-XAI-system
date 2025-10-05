# Calibration Notes (v1)

**Scope.** COFFEE scenarios (milk, yogurts, cheeses) with three options per scenario.

**Data provenance (no UGC ingestion).** Attribute ranges and phrasing calibrated from *public* brand claims and Instagram advertising discourse (aggregate review of posts/ads; no copying of user comments; no direct quotes). Sources inform only value ranges and wording; all scenario rows are synthetic.

**Units & ranges.**
- Price (CAD): ~2.5–8.0 (per item/pack as listed)
- Carbon intensity (kgCO₂e/kg): milk ≈ 1–3; yogurts ≈ 2–4; cheeses ≈ 9–13
- Animal welfare / Transparency / Labour: 0–100 (ordinal proxies)
- Packaging recyclability: 40–95%

**Mapping from ad claims to attributes (examples).**
| Claim fragment (public ad) | Attribute(s) affected | Calibration note |
|---|---|---|
| “locally sourced”, “co-op” | Transparency, Labour | +10–20 uplift vs. generic baseline |
| “organic”, “pasture-raised” | Animal welfare | +15–25 uplift; price +0.5–1.5 CAD |
| “fully recyclable pack” | Packaging | set ≥80% (cap at 95%) |
| “lower carbon footprint” | Carbon | −0.2 to −0.4 kgCO₂e/kg vs. category mean |

**Normalization policy.** Min–max within scenario to equalize attribute influence across options.

**Ethical note.** Scenarios are synthetic and for research only. No individual user data are stored or processed.


