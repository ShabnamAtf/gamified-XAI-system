% prolog/coffee/utilitarian_mcda.pl
:- module(utilitarian_mcda, [
    weights_default/1,
    score_round/3,
    best_option/4,
    demo_csv/1                % demo_csv(+OutCsvAbsPath)
]).

:- use_module('coffee_data', [round_options/2, load_coffee_data/0]).
:- use_module(library(lists)).
:- use_module(library(filesex)).  % make_directory_path/1

% -------- weights (list of Key-Weight) --------
weights_default([
    price_cad_per_cup-(-0.25),
    carbon_g_co2e_per_cup-(-0.15),
    water_l_per_cup-(-0.05),
    transparency_score-0.15,
    farmer_income_share_pct-0.15,
    deforestation_risk-(-0.10),
    child_labor_risk-(-0.20),
    packaging_recyclability-0.05,
    shade_cert-0.05,
    taste_score_sca-0.10,
    freshness_days_since_roast-(-0.03),
    brew_time_min-(-0.02)
]).

fields([
    price_cad_per_cup,
    carbon_g_co2e_per_cup,
    water_l_per_cup,
    transparency_score,
    farmer_income_share_pct,
    deforestation_risk,
    child_labor_risk,
    packaging_recyclability,
    shade_cert,
    taste_score_sca,
    freshness_days_since_roast,
    brew_time_min
]).

% -------- public API --------
score_round(RoundIdx, Weights, Scores) :-
    round_options(RoundIdx, Os0),
    maplist(derive_features, Os0, Os),
    bounds_for_round(Os, Bnds),
    maplist(score_one(Weights, Bnds), Os, Pairs),
    sort(2, @>=, Pairs, Scores).  % OptionID-Score, descending

best_option(RoundIdx, Weights, Best, Score) :-
    score_round(RoundIdx, Weights, [OptionID-Score|_]),
    round_options(RoundIdx, Os),
    member(Best, Os),
    Best.option_id == OptionID.

% -------- scoring internals --------
score_one(Weights, Bnds, D, ID-Score) :-
    ID = D.option_id,
    fields(Fs),
    foldl(accum_score(Weights, Bnds, D), Fs, 0.0, Score).

accum_score(Ws, B, D, F, Acc, Out) :-
    safe_weight(Ws, F, Wf),
    safe_feature(F, D, Raw),
    normalize(F, Raw, B, N),
    Out is Acc + (Wf * N).

safe_weight(Ws, F, Wf) :-
    ( weight_lookup(Ws, F, V), number(V) -> Wf = V ; Wf = 0.0 ).

weight_lookup([K-V|_], K, V) :- !.
weight_lookup([_|T], K, V) :- weight_lookup(T, K, V).

safe_feature(F, D, V) :-
    ( get_dict(F, D, Raw), to_number_strict(Raw, V) -> true ; V = 0.0 ).

derive_features(D0, D) :-
    ( D0.shade_cert == true  -> Shade = 1
    ; D0.shade_cert == false -> Shade = 0
    ; Shade = 0
    ),
    ( number(D0.packaging_recyclability) -> Pack = D0.packaging_recyclability ; Pack = 0 ),
    D = D0.put(_{shade_cert:Shade, packaging_recyclability:Pack}).

bounds_for_round(Os, Bnds) :-
    fields(Fs),
    maplist(field_bounds(Os), Fs, Pairs),
    dict_create(Bnds, bnds, Pairs).

field_bounds(Os, F, F-_{min:Min, max:Max}) :-
    findall(V, (member(D, Os), get_dict(F, D, Raw), to_number_strict(Raw, V)), Vs0),
    include(number, Vs0, Vs),
    ( Vs = [] -> Min=0, Max=1
    ; min_list(Vs, Min0), max_list(Vs, Max0),
      ( Min0 =:= Max0 -> Min is Min0, Max is Min0+1
      ; Min=Min0, Max=Max0
      )
    ).

normalize(_F, Raw, _Bnds, 0.5) :- var(Raw), !.
normalize(F, Raw, Bnds, N) :-
    get_dict(F, Bnds, _{min:Min, max:Max}),
    ( Max =:= Min -> N is 0.5
    ; N is (Raw - Min) / (Max - Min)
    ).

to_number_strict(V, V) :- number(V), !.
to_number_strict(true, 1) :- !.
to_number_strict(false, 0) :- !.
to_number_strict('True', 1) :- !.
to_number_strict('False', 0) :- !.
to_number_strict("True", 1) :- !.
to_number_strict("False", 0) :- !.
to_number_strict(V, N) :- atom(V),   catch(atom_number(V, N),   _, fail), !.
to_number_strict(V, N) :- string(V), catch(number_string(N, V), _, fail), !.
to_number_strict(_, _) :- fail.

% -------- demo: write ALL rounds (1..6) into one CSV --------
demo_csv(OutCsvAbsPath) :-
    load_coffee_data,
    weights_default(W),
    file_directory_name(OutCsvAbsPath, OutDir),
    ( exists_directory(OutDir) -> true ; make_directory_path(OutDir) ),
    setup_call_cleanup(
        open(OutCsvAbsPath, write, S),
        (
            format(S, "round_idx,option_id,score,is_best~n", []),
            forall(between(1,6,R),
                (
                    score_round(R, W, Scores),
                    Scores = [BestID-_|_],
                    forall(member(OptID-Sc, Scores),
                        (   (OptID == BestID -> Flag = 1 ; Flag = 0),
                            format(S, "~d,~w,~2f,~d~n", [R, OptID, Sc, Flag])
                        )
                    )
                )
            )
        ),
        close(S)
    ).
