% prolog/coffee/policy_trace_text.pl
:- module(policy_trace_text, [
    human_readable_trace_csv/2,        % human_readable_trace_csv(+OutCsvAbsPath, +Eps)
    human_readable_trace_csv_with/3    % human_readable_trace_csv_with(+OutCsvAbsPath, +Eps, +Weights)
]).

:- use_module(library(lists)).
:- use_module(library(filesex)).

:- use_module('coffee_data', [round_options/2, find_option/2, load_coffee_data/0]).
:- use_module('kantian_rules_coffee', [violations_for/2]).
:- use_module('utilitarian_mcda', [weights_default/1, score_round/3]).

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

human_readable_trace_csv(OutCsvAbsPath, Eps) :-
    load_coffee_data,
    weights_default(W),
    human_readable_trace_csv_with(OutCsvAbsPath, Eps, W).

human_readable_trace_csv_with(OutCsvAbsPath, Eps, W) :-
    load_coffee_data,
    ensure_outdir(OutCsvAbsPath),
    setup_call_cleanup(
        open(OutCsvAbsPath, write, S),
        (
            format(S, "round_idx,option_id,product_name,score,rank,kantian_status,violations,drivers,decision_note~n", []),
            forall(between(1,6,R), write_round(S, R, W, Eps))
        ),
        close(S)
    ).

write_round(S, R, W, Eps) :-
    score_round(R, W, Scores),
    Scores = [BestID-BestScore|_],
    find_option(BestID, BestD),
    violations_for(BestD, Vbest),
    (   Vbest == [] ->
        PickID = BestID, PickScore = BestScore, Switched = 0
    ;   Threshold is BestScore - Eps,
        findall(OptID-Sc,
                ( member(OptID-Sc, Scores),
                  Sc >= Threshold,
                  find_option(OptID, D),
                  violations_for(D, V), V == []
                ),
                CleanCandidates),
        (   CleanCandidates = [] ->
            PickID = BestID, PickScore = BestScore, Switched = 0
        ;   sort(2, @>=, CleanCandidates, [PickID-PickScore|_]), Switched = 1
        )
    ),
    round_options(R, Os),
    bounds_for_round(R, Bnds),
    forall(member(D, Os),
        (
            ID = D.option_id,
            product_name_string(D.product_name, PName0),
            csv_sanitize(PName0, PName),
            score_lookup(Scores, ID, Sc),
            rank_lookup(Scores, ID, Rank),
            violations_for(D, Vs),
            ( Vs == [] -> Kant = clean ; Kant = violations ),
            vio_list_string(Vs, VioStr0), csv_sanitize(VioStr0, VioStr),
            drivers_for(D, W, Bnds, DriversStr0), csv_sanitize(DriversStr0, DriversStr),
            decision_note(ID, PickID, Switched, BestID, PickScore, BestScore, Eps, Vbest, Note0),
            csv_sanitize(Note0, Note),
            format(S, "~d,~w,\"~s\",~2f,~d,~w,\"~s\",\"~s\",\"~s\"~n",
                   [R, ID, PName, Sc, Rank, Kant, VioStr, DriversStr, Note])
        )
    ).

drivers_for(D, W, Bnds, OutStr) :-
    fields(Fs),
    findall(F-Contrib,
        ( member(F, Fs),
          weight_lookup(W, F, WF),
          feature_value(F, D, Raw0),
          to_number_strict(Raw0, Raw),
          normalize(F, Raw, Bnds, N),
          Contrib is WF * N
        ),
        Pairs0),
    maplist(add_abs, Pairs0, PairsAbs),
    sort(2, @>=, PairsAbs, SortedAbs),  % F-(Abs,Contrib)
    top_k(SortedAbs, 5, Top5),
    maplist(contrib_to_token, Top5, Tokens),
    atomic_list_concat(Tokens, '; ', A),
    atom_string(A, OutStr).

add_abs(F-C, F-(Abs,C)) :- Abs is abs(C).

top_k(List, K, Take) :-
    length(Prefix, K),
    append(Prefix, _, List), !, Take = Prefix.
top_k(List, _K, List).

contrib_to_token(F-(Abs,Contrib), Tok) :-
    short_name(F, Short),
    ( Contrib >= 0 -> Prefix = '+' ; Prefix = '-' ),
    format(atom(Tok), "~w~w(~2f)", [Prefix, Short, Abs]).

short_name(price_cad_per_cup,         price).
short_name(carbon_g_co2e_per_cup,     carbon).
short_name(water_l_per_cup,           water).
short_name(transparency_score,        transp).
short_name(farmer_income_share_pct,   income).
short_name(deforestation_risk,        deforest).
short_name(child_labor_risk,          child).
short_name(packaging_recyclability,   pack_rec).
short_name(shade_cert,                shade).
short_name(taste_score_sca,           taste).
short_name(freshness_days_since_roast,fresh).
short_name(brew_time_min,             brew_time).

decision_note(ID, PickID, Switched, BestID, PickScore, BestScore, Eps, Vbest, Note) :-
    ( ID \== PickID ->
        Note = "-"
    ; Switched =:= 1 ->
        list_to_semicolon(Vbest, VbestStr),
        Regret is BestScore - PickScore,
        format(atom(Note), "picked (switched from=~w; regret=~2f; eps=~2f; best_viol=~w)",
               [BestID, Regret, Eps, VbestStr])
    ; Note = "picked (kept util_best)"
    ).

bounds_for_round(R, Bnds) :-
    round_options(R, Os),
    fields(Fs),
    maplist(field_bounds(Os), Fs, Pairs),
    dict_create(Bnds, bnds, Pairs).

field_bounds(Os, F, F-_{min:Min, max:Max}) :-
    findall(V, (member(D, Os), feature_value(F, D, Raw), to_number_strict(Raw, V)), Vs0),
    include(number, Vs0, Vs),
    ( Vs = [] -> Min=0, Max=1
    ; min_list(Vs, Min0), max_list(Vs, Max0),
      ( Min0 =:= Max0 -> Min is Min0, Max is Min0+1
      ; Min=Min0, Max=Max0
      )
    ).

normalize(_F, Raw, _B, 0.5) :- var(Raw), !.
normalize(F, Raw, Bnds, N) :-
    get_dict(F, Bnds, _{min:Min, max:Max}),
    ( Max =:= Min -> N is 0.5
    ; N is (Raw - Min) / (Max - Min)
    ).

feature_value(F, D, V) :- get_dict(F, D, V).

weight_lookup([K-V|_], K, V) :- !.
weight_lookup([_|T], K, V) :- weight_lookup(T, K, V).
weight_lookup([], _, 0.0).

score_lookup([ID-Sc|_], ID, Sc) :- !.
score_lookup([_|T], ID, Sc) :- score_lookup(T, ID, Sc).
score_lookup([], _, 0.0).

rank_lookup(Scores, ID, Rank) :-
    ( nth1(Rank, Scores, ID-_) -> true ; Rank = 0 ).

vio_list_string([], "") :- !.
vio_list_string(L, S) :- atomic_list_concat(L, ';', A), atom_string(A, S).

list_to_semicolon([], "") :- !.
list_to_semicolon(L, S) :-
    maplist(atom_string, L, Ss), atomic_list_concat(Ss, ';', A), atom_string(A, S).

product_name_string(Name, S) :-
    ( string(Name) -> S = Name
    ; atom(Name)   -> atom_string(Name, S)
    ; S = ""
    ).

csv_sanitize(Value, Out) :-
    ( string(Value) -> S0 = Value
    ; atom(Value)   -> atom_string(Value, S0)
    ; term_string(Value, S0)
    ),
    split_string(S0, "\"", "", Parts),
    atomic_list_concat(Parts, "'", A),
    atom_string(A, Out).

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

ensure_outdir(Path) :-
    file_directory_name(Path, Dir),
    ( exists_directory(Dir) -> true ; make_directory_path(Dir) ).
