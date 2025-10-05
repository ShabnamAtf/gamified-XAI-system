% prolog/coffee/meta_explainer.pl
:- module(meta_explainer, [
    meta_decision_for_round/4,      % meta_decision_for_round(+RoundIdx, +Weights, +Eps, -Decision:dict)
    meta_csv/2,                     % meta_csv(+OutCsvAbsPath, +Eps)                [uses default weights]
    meta_trace_csv/2,               % meta_trace_csv(+OutCsvAbsPath, +Eps)          [uses default weights]
    meta_csv_with/3,                % meta_csv_with(+OutCsvAbsPath, +Eps, +Weights) [custom weights]
    meta_trace_csv_with/3           % meta_trace_csv_with(+OutCsvAbsPath, +Eps, +Weights)
]).

:- use_module('coffee_data', [round_options/2, find_option/2, load_coffee_data/0]).
:- use_module('utilitarian_mcda', [weights_default/1, score_round/3]).
:- use_module('kantian_rules_coffee', [violations_for/2]).
:- use_module(library(lists)).
:- use_module(library(filesex)).

meta_decision_for_round(RoundIdx, Weights, Eps, Dec) :-
    score_round(RoundIdx, Weights, Scores),
    Scores = [BestID-BestScore | _],
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
        ;   sort(2, @>=, CleanCandidates, [PickID-PickScore | _]), Switched = 1
        )
    ),
    round_options(RoundIdx, Os),
    include(is_clean, Os, CleanOs),
    length(Os, NOpts),
    length(CleanOs, NClean),
    list_to_semicolon(Vbest, VStr),
    Regret is BestScore - PickScore,
    Dec = _{
        round_idx:RoundIdx,
        util_best_id:BestID,
        util_best_score:BestScore,
        util_best_violations:VStr,
        picked_id:PickID,
        picked_score:PickScore,
        regret:Regret,
        switched:Switched,
        eps:Eps,
        num_options:NOpts,
        num_clean:NClean
    }.

is_clean(D) :-
    violations_for(D, Vs), Vs == [].

list_to_semicolon([], "") :- !.
list_to_semicolon(L, S) :-
    maplist(atom_string, L, Ss), atomic_list_concat(Ss, ';', A), atom_string(A, S).

% ---------- CSV (default weights) ----------
meta_csv(OutCsvAbsPath, Eps) :-
    load_coffee_data,
    weights_default(W),
    meta_csv_with(OutCsvAbsPath, Eps, W).

meta_trace_csv(OutCsvAbsPath, Eps) :-
    load_coffee_data,
    weights_default(W),
    meta_trace_csv_with(OutCsvAbsPath, Eps, W).

% ---------- CSV (custom weights) ----------
meta_csv_with(OutCsvAbsPath, Eps, W) :-
    load_coffee_data,                         % <<< ensure data is loaded
    ensure_outdir(OutCsvAbsPath),
    setup_call_cleanup(
        open(OutCsvAbsPath, write, S),
        (
            format(S, "round_idx,util_best_id,util_best_score,util_best_violations,picked_id,picked_score,regret,switched,eps,num_options,num_clean~n", []),
            forall(between(1,6,R),
                ( meta_decision_for_round(R, W, Eps, D),
                  format(S, "~d,~w,~2f,~w,~w,~2f,~2f,~d,~2f,~d,~d~n",
                         [ D.round_idx,
                           D.util_best_id,
                           D.util_best_score,
                           D.util_best_violations,
                           D.picked_id,
                           D.picked_score,
                           D.regret,
                           D.switched,
                           D.eps,
                           D.num_options,
                           D.num_clean
                         ])
                )
            )
        ),
        close(S)
    ).

meta_trace_csv_with(OutCsvAbsPath, Eps, W) :-
    load_coffee_data,                         % <<< ensure data is loaded
    ensure_outdir(OutCsvAbsPath),
    setup_call_cleanup(
        open(OutCsvAbsPath, write, S),
        (
            format(S, "round_idx,option_id,product_name,score,rank,kantian_status,violations,is_util_best,is_picked,reason~n", []),
            forall(between(1,6,R), write_round_trace(S, R, W, Eps))
        ),
        close(S)
    ).

write_round_trace(S, R, W, Eps) :-
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
    list_to_semicolon(Vbest, VbestStr),
    Regret is BestScore - PickScore,
    round_options(R, Os),
    forall(member(D, Os),
        (
            ID = D.option_id,
            product_name_string(D.product_name, PName),
            score_lookup(Scores, ID, Sc),
            rank_lookup(Scores, ID, Rank),
            violations_for(D, Vs),
            ( Vs == [] -> Kant = clean ; Kant = violations ),
            list_to_semicolon(Vs, VStr),
            ( ID == BestID -> IsBest = 1 ; IsBest = 0 ),
            ( ID == PickID -> IsPicked = 1 ; IsPicked = 0 ),
            reason_for_row(ID, PickID, Switched, BestID, VbestStr, Regret, Eps, Reason),
            format(S, "~d,~w,~w,~2f,~d,~w,~w,~d,~d,~w~n",
                   [R, ID, PName, Sc, Rank, Kant, VStr, IsBest, IsPicked, Reason])
        )
    ).

product_name_string(Name, S) :-
    ( string(Name) -> S = Name
    ; atom(Name)   -> atom_string(Name, S)
    ; S = ""
    ).

score_lookup([ID-Sc|_], ID, Sc) :- !.
score_lookup([_|T], ID, Sc) :- score_lookup(T, ID, Sc).
score_lookup([], _, 0.0).

rank_lookup(Scores, ID, Rank) :-
    ( nth1(Rank, Scores, ID-_) -> true ; Rank = 0 ).

reason_for_row(ID, PickID, Switched, BestID, BestViolStr, Regret, Eps, Reason) :-
    ( ID \== PickID -> Reason = "-"
    ; Switched =:= 1 ->
        format(atom(Reason), "switched_from=~w; regret=~2f; eps=~2f; best_violations=~w",
               [BestID, Regret, Eps, BestViolStr])
    ; format(atom(Reason), "kept_util_best", [])
    ).

ensure_outdir(Path) :-
    file_directory_name(Path, Dir),
    ( exists_directory(Dir) -> true ; make_directory_path(Dir) ).
