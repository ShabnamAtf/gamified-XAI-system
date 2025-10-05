% prolog/coffee/ui_rationales.pl
:- module(ui_rationales, [
    rationales_csv/4   % rationales_csv(+OutCsvAbsPath, +Eps, +Trials, +NoiseScale)
]).

:- use_module(library(lists)).
:- use_module(library(filesex)).

:- use_module('coffee_data', [round_options/2, find_option/2, load_coffee_data/0]).
:- use_module('utilitarian_mcda', [weights_default/1, score_round/3]).
:- use_module('kantian_rules_coffee', [violations_for/2]).
:- use_module('meta_explainer', [meta_decision_for_round/4]).
:- use_module('confidence_meta', [confidence_for_round/5]).

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

rationales_csv(OutCsvAbsPath, Eps, Trials, NoiseScale) :-
    load_coffee_data,
    ensure_outdir(OutCsvAbsPath),
    weights_default(W),
    setup_call_cleanup(
        open(OutCsvAbsPath, write, S),
        (
            format(S, "round_idx,picked_id,product_name,util_score,rank,drivers,kantian_status,violations,decision_note,confidence,ask_more_info~n", []),
            forall(between(1,6,R), write_row(S, R, W, Eps, Trials, NoiseScale))
        ),
        close(S)
    ).

write_row(S, R, W, Eps, Trials, NoiseScale) :-
    score_round(R, W, Scores),
    meta_decision_for_round(R, W, Eps, Dec),
    PickID = Dec.picked_id,
    PickScore = Dec.picked_score,
    _BestID = Dec.util_best_id,
    BestScore = Dec.util_best_score,
    find_option(PickID, D),
    product_name_string(D.product_name, PName0), csv_sanitize(PName0, PName),
    rank_lookup(Scores, PickID, Rank),
    bounds_for_round(R, Bnds),
    drivers_for(D, W, Bnds, Drivers0), csv_sanitize(Drivers0, Drivers),
    violations_for(D, Vs),
    ( Vs == [] -> Kant = clean ; Kant = violations ),
    vio_list_string(Vs, VStr0), csv_sanitize(VStr0, VStr),
    decision_note(PickID, Dec, BestScore, Eps, Note0), csv_sanitize(Note0, Note),
    confidence_for_round(R, Eps, Trials, NoiseScale, C),
    Conf is C.confidence,
    Ask = C.ask_more_info,
    format(S, "~d,~w,\"~s\",~2f,~d,\"~s\",~w,\"~s\",\"~s\",~2f,~d~n",
           [R, PickID, PName, PickScore, Rank, Drivers, Kant, VStr, Note, Conf, Ask]).

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
    sort(2, @>=, PairsAbs, SortedAbs),
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

rank_lookup(Scores, ID, Rank) :-
    ( nth1(Rank, Scores, ID-_) -> true ; Rank = 0 ).

vio_list_string([], "") :- !.
vio_list_string(L, S) :- atomic_list_concat(L, ';', A), atom_string(A, S).

decision_note(PickID, Dec, BestScore, Eps, Note) :-
    ( Dec.switched =:= 1 ->
        list_to_semicolon(Dec.util_best_violations, VbestStr),
        Regret is BestScore - Dec.picked_score,
        ( PickID == Dec.picked_id ->
            format(atom(Note), "picked (switched from=~w; regret=~2f; eps=~2f; best_viol=~w)",
                   [Dec.util_best_id, Regret, Eps, VbestStr])
        ;   Note = "-" )
    ; ( PickID == Dec.picked_id ->
          Note = "picked (kept util_best)"
      ;   Note = "-"
      )
    ).

list_to_semicolon("", "") :- !.
list_to_semicolon(S, S)  :- string(S), !.
list_to_semicolon(L, S)  :-
    is_list(L), maplist(atom_string, L, Ss), atomic_list_concat(Ss, ';', A), atom_string(A, S).

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
