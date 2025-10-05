# meta_explainer.py
# Drop-in meta-explainer for MCDA + Kantian constraints with regret checks.

from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple

# --------- Data structures ---------------------------------------------------

@dataclass
class Candidate:
    """One option competing to be chosen."""
    option_id: str
    name: str
    mcda_score: float                       # higher is better
    util_drivers: Dict[str, float]          # signed contributions (e.g., {"child": -0.20, "carbon": -0.15})
    kantian_status: str                     # "ok" or "violations"
    kantian_rules: List[str]                # e.g., ["r1", "r3"]
    confidence: Optional[float] = None      # optional: model confidence 0..1
    uncertainty: Optional[Dict[str, float]] = None  # e.g., {"self_consistency_var": 0.12}

RULE_MAP: Dict[str, str] = {
    "r1": "Child/forced labour",
    "r3": "Deforestation risk (or missing shade-grown)",
    "r4": "Farmer income floor (LIRP)",
    "r5": "Supply-chain transparency",
    "r6": "Process safety (e.g., controversial decaf solvent)",
    # add more if you use r2, r7, ...
}

# --------- Helpers -----------------------------------------------------------

def _sorted_by_score(cands: List[Candidate]) -> List[Candidate]:
    return sorted(cands, key=lambda c: c.mcda_score, reverse=True)

def _regret(best: Candidate, second: Optional[Candidate]) -> float:
    """Relative margin between best and runner-up (0..1). Larger -> safer lead."""
    if second is None:
        return 1.0
    denom = abs(best.mcda_score) if abs(best.mcda_score) > 1e-9 else 1.0
    return max(0.0, (best.mcda_score - second.mcda_score) / denom)

def _top_drivers(drivers: Dict[str, float], k_pos=2, k_neg=2) -> Tuple[List[Tuple[str, float]], List[Tuple[str, float]]]:
    pos = sorted([(k, v) for k, v in drivers.items() if v > 0], key=lambda x: -x[1])[:k_pos]
    neg = sorted([(k, v) for k, v in drivers.items() if v < 0], key=lambda x: x[1])[:k_neg]
    return pos, neg

def _fmt_rules(rule_ids: List[str]) -> str:
    names = [RULE_MAP.get(r, r) for r in rule_ids]
    return ", ".join(names) if names else "—"

# --------- Meta decision logic ----------------------------------------------

def meta_explain(
    candidates: List[Candidate],
    regret_bound: float = 0.20,                   # if margin < 20%, call it a "close call"
    kantian_override_margin: float = 0.25,        # within 25% margin prefer a Kantian-safe alternative
    low_conf: float = 0.45,                       # optional thresholds if provided
    high_uncert: float = 0.25,                    # e.g., self_consistency variance
) -> Dict:
    """
    Returns a dict with keys: action, recommended_id, summary, details.
    actions: 'recommend', 'compare', 'block'
    """
    if not candidates:
        return {"action": "block", "recommended_id": None, "summary": "No options provided.", "details": {}}

    ranked = _sorted_by_score(candidates)
    best = ranked[0]
    second = ranked[1] if len(ranked) > 1 else None
    regret = _regret(best, second)

    # Uncertainty heuristics (only if values exist)
    flags: List[str] = []
    if best.confidence is not None and best.confidence < low_conf:
        flags.append(f"low_conf({best.confidence:.2f})")
    if best.uncertainty and "self_consistency_var" in best.uncertainty:
        if best.uncertainty["self_consistency_var"] > high_uncert:
            flags.append(f"high_self_consistency_var({best.uncertainty['self_consistency_var']:.2f})")

    # A) Best violates Kantian rules -> look for a safe alternative within override margin
    if best.kantian_status == "violations":
        safe_alts = [c for c in ranked[1:] if c.kantian_status == "ok"]
        if safe_alts:
            alt = safe_alts[0]  # top safe
            score_gap = (best.mcda_score - alt.mcda_score) / (abs(best.mcda_score) if abs(best.mcda_score) > 1e-9 else 1.0)
            if score_gap <= kantian_override_margin:
                pos, neg = _top_drivers(alt.util_drivers)
                summary = (
                    f"We DO NOT recommend '{best.name}' because it violates: {_fmt_rules(best.kantian_rules)}. "
                    f"Within a {int(kantian_override_margin*100)}% margin, the safest alternative is '{alt.name}'."
                )
                details = {
                    "best": best.__dict__,
                    "alternative": alt.__dict__,
                    "reason": "kantian_override",
                    "regret_vs_alt": max(0.0, (alt.mcda_score - best.mcda_score) / (abs(alt.mcda_score) if abs(alt.mcda_score) > 1e-9 else 1.0)),
                    "top_util_drivers_alt_positive": pos,
                    "top_util_drivers_alt_negative": neg,
                }
                return {"action": "recommend", "recommended_id": alt.option_id, "summary": summary, "details": details}

        return {"action": "block", "recommended_id": None,
                "summary": f"Block: Top-scoring option '{best.name}' violates: {_fmt_rules(best.kantian_rules)}. "
                           f"No Kantian-safe alternative within the allowed margin.",
                "details": {"best": best.__dict__, "reason": "kantian_block"}}

    # B) Best is Kantian-safe
    pos, neg = _top_drivers(best.util_drivers)
    if regret < regret_bound or flags:
        runner = second.name if second else "—"
        tag = "; ".join(flags) if flags else "close_scores"
        summary = (
            f"Tentative: '{best.name}' leads, but margin is small (regret={regret:.2f})"
            f" or uncertainty flags present ({tag}). Consider comparing with '{runner}'."
        )
        details = {
            "best": best.__dict__,
            "runner_up": second.__dict__ if second else None,
            "reason": "close_call_or_uncertain",
            "regret": regret,
            "top_util_drivers_best_positive": pos,
            "top_util_drivers_best_negative": neg,
        }
        return {"action": "compare", "recommended_id": best.option_id, "summary": summary, "details": details}

    # C) Clear win, Kantian-safe
    summary = (
        f"Recommend: '{best.name}' (clear lead; regret={regret:.2f}). "
        f"Top positive drivers: {', '.join([f'{k}(+{v:.2f})' for k, v in pos])}; "
        f"Top penalties: {', '.join([f'{k}({v:.2f})' for k, v in neg])}."
    )
    details = {
        "best": best.__dict__,
        "runner_up": second.__dict__ if second else None,
        "reason": "clear_lead",
        "regret": regret,
        "top_util_drivers_best_positive": pos,
        "top_util_drivers_best_negative": neg,
    }
    return {"action": "recommend", "recommended_id": best.option_id, "summary": summary, "details": details}

# --------- Optional template bridge -----------------------------------------

def to_template_vars(meta_output: Dict) -> Dict[str, str]:
    """
    Convert the meta_explain output into a flat dict you can feed to your
    CSV-based explanation templates (e.g., using str.format_map()).
    """
    details = meta_output.get("details", {}) or {}
    best = details.get("best", {}) or {}
    runner = details.get("runner_up", {}) or {}

    vars_ = {
        "action": meta_output.get("action", ""),
        "summary": meta_output.get("summary", ""),
        "best_name": best.get("name", ""),
        "best_id": best.get("option_id", ""),
        "best_score": f"{best.get('mcda_score', float('nan')):.3f}" if "mcda_score" in best else "",
        "runner_name": runner.get("name", ""),
        "runner_id": runner.get("option_id", ""),
        "runner_score": f"{runner.get('mcda_score', float('nan')):.3f}" if "mcda_score" in runner else "",
        "kantian_status": best.get("kantian_status", ""),
        "kantian_rules": ", ".join(best.get("kantian_rules", [])),
        "regret": f"{details.get('regret', float('nan')):.2f}" if "regret" in details else "",
        "reason": details.get("reason", ""),
    }
    return vars_

