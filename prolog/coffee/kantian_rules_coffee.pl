% prolog/coffee/kantian_rules_coffee.pl
:- module(kantian_rules_coffee, [ violations_for/2 ]).

% R1: Ban on child/forced labor
viol_r1(D) :- D.child_labor_risk > 0.2.

% R2: Ban on high-risk deforestation (only if no shade)
viol_r2(D) :- D.deforestation_risk > 0.6, D.shade_cert == false.

% R3: Minimum transparency
viol_r3(D) :- D.transparency_score < 0.5.

% R4: “Decent income”
viol_r4(D) :- D.farmer_income_share_pct < 20.

% R5: Decaf safety
viol_r5(D) :- D.decaf_process == 'solvent_risky'.

% R6: Irresponsible packaging
viol_r6(D) :- D.packaging_recyclability =:= 0, D.packaging_type == 'plastic-film'.

violations_for(D, Vs) :-
    findall(V,
        ( member(V-Pred, [r1-viol_r1, r2-viol_r2, r3-viol_r3, r4-viol_r4, r5-viol_r5, r6-viol_r6]),
          Goal =.. [Pred, D], call(Goal)
        ),
    Vs).
