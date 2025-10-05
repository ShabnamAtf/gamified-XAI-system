% prolog/coffee/confidence_meta.pl
:- module(confidence_meta, [
    confidence_for_round/5,       % confidence_for_round(+RoundIdx,+Eps,+Trials,+NoiseScale,-Dict)
    confidence_csv/4              % confidence_csv(+OutCsvAbsPath,+Eps,+Trials,+NoiseScale)
]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(filesex)).

:- use_module('coffee_data', [round_options/2, find_option/2, load_coffee_data/0]).
:- use_module('utilitarian_mcda', [weights_default/1, score_round/3]).
:- use_module('meta_explainer', [meta_decision_for_round/4]).
:- use_module('kantian_rules_coffee', [violations_for/2]).

% -------- public --------
confidence_for_round(R, Eps, Trials, NoiseScale, Out) :-
    weights_default(W),
    score_round(R, W, Scores),                 % [ID-Sc] sorted desc
    Scores = [_BestID-BestScore|Rest],
    second_score(Rest, SecondBest),            % handle 1-option edge
    softmax_entropy(Scores, 1.0, EntropyN),    % normalized 0..1
    Margin is BestScore - SecondBest,
    meta_decision_for_round(R, W, Eps, Dec),   % meta pick under default W
    PickID = Dec.picked_id,
    self_consistency(R, Eps, Trials, NoiseScale, PickID, Stability),
    avg_transparency(R, AvgTransp),
    ( EntropyN > 0.70 -> RE1 = ['high_entropy'] ; RE1 = [] ),
    ( Stability < 0.60 -> RE2 = ['low_stability'|RE1] ; RE2 = RE1 ),
    ( Margin < 0.05 -> RE3 = ['small_margin'|RE2] ; RE3 = RE2 ),
    ( AvgTransp < 0.50 -> RE4 = ['low_transparency'|RE3] ; RE4 = RE3 ),
    reasons_string(RE4, AskReasons),
    ( RE4 == [] -> AskFlag = 0 ; AskFlag = 1 ),
    MarginClamped is max(0.0, min(1.0, Margin / 0.5)),
    Conf is 0.4*(1.0-EntropyN) + 0.4*Stability + 0.2*MarginClamped,
    Out = _{
        round_idx:R,
        picked_id:PickID,
        entropy:EntropyN,
        margin:Margin,
        stability:Stability,
        avg_transparency:AvgTransp,
        confidence:Conf,
        ask_more_info:AskFlag,
        ask_reasons:AskReasons
    }.

confidence_csv(OutCsvAbsPath, Eps, Trials, NoiseScale) :-
    load_coffee_data,
    file_directory_name(OutCsvAbsPath, OutDir),
    ( exists_directory(OutDir) -> true ; make_directory_path(OutDir) ),
    setup_call_cleanup(
        open(OutCsvAbsPath, write, S),
        (
            format(S, "round_idx,picked_id,entropy,margin,stability,avg_transparency,confidence,ask_more_info,ask_reasons~n", []),
            forall(between(1,6,R),
                ( confidence_for_round(R, Eps, Trials, NoiseScale, D),
                  format(S, "~d,~w,~4f,~4f,~4f,~4f,~4f,~d,~w~n",
                         [ D.round_idx, D.picked_id, D.entropy, D.margin, D.stability,
                           D.avg_transparency, D.confidence, D.ask_more_info, D.ask_reasons ])
                )
            )
        ),
        close(S)
    ).

% -------- helpers --------
second_score([_-S|_], S) :- !.
second_score([], 0.0).

softmax_entropy(Scores, Tau, Hn) :-
    maplist([_-S, E]>>(Z is S/Tau, E is exp(Z)), Scores, Es),
    sum_list(Es, SumE),
    ( SumE =:= 0 -> Hn = 1.0
    ; maplist([E,P]>>(P is E/SumE), Es, Ps),
      entropy(Ps, H),
      length(Ps, K), (K =:= 0 -> Hn = 1.0 ; Hn is H / log(K))
    ).

entropy(Ps, H) :-
    foldl(ent_step, Ps, 0.0, HNeg),
    H is -HNeg.
ent_step(P, Acc, Out) :-
    ( P =< 0 -> Out = Acc
    ; Out is Acc + P*log(P)
    ).

self_consistency(R, Eps, Trials, NoiseScale, BasePick, Stability) :-
    weights_default(W0),
    self_consistency_trials(Trials, R, Eps, NoiseScale, W0, BasePick, 0, Hit),
    ( Trials =:= 0 -> Stability = 1.0
    ; Stability is Hit / Trials
    ).

self_consistency_trials(0, _R, _Eps, _Ns, _W0, _BasePick, HitAcc, HitAcc) :- !.
self_consistency_trials(N, R, Eps, Ns, W0, BasePick, HitAcc, HitOut) :-
    N > 0,
    perturb_weights(W0, Ns, W1),
    meta_decision_for_round(R, W1, Eps, Dec1),
    ( Dec1.picked_id == BasePick -> HitAcc1 is HitAcc + 1 ; HitAcc1 = HitAcc ),
    N1 is N - 1,
    self_consistency_trials(N1, R, Eps, Ns, W0, BasePick, HitAcc1, HitOut).

perturb_weights([], _Ns, []).
perturb_weights([K-V|T], Ns, [K-V2|T2]) :-
    U is random_float,                       % 0..1
    N0 is (2.0*U - 1.0),                    % -1..1
    Scale is 1.0 + Ns * N0,                 % (1 +/- Ns)
    V2 is V * Scale,
    perturb_weights(T, Ns, T2).

avg_transparency(R, Avg) :-
    round_options(R, Os),
    findall(T, (member(D,Os), get_dict(transparency_score, D, T)), Ts),
    ( Ts == [] -> Avg = 0.0 ; sum_list(Ts, Sum), length(Ts, K), Avg is Sum / K ).

reasons_string([], "") :- !.
reasons_string(L, S) :-
    atomic_list_concat(L, ';', A), atom_string(A, S).
