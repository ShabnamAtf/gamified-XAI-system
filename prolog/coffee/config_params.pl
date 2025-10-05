% prolog/coffee/config_params.pl
:- module(config_params, [
    load_weights_or_default/1,   % -Weights:list(Key-Weight)
    load_eps_or_default/1        % -Eps:float
]).

:- use_module(library(csv)).
:- use_module(library(lists)).

:- use_module('utilitarian_mcda', [weights_default/1]).

weights_csv('data/coffee/weights.csv').
params_csv('data/coffee/params.csv').

load_weights_or_default(W) :-
    weights_csv(F),
    ( exists_file(F) ->
        catch(read_weights_csv(F, W0), _, fail) -> W = W0
    ;   fail
    ),
    !.
load_weights_or_default(W) :-
    weights_default(W).

load_eps_or_default(Eps) :-
    params_csv(F),
    ( exists_file(F) ->
        catch(read_eps_csv(F, Eps0), _, fail) -> Eps = Eps0
    ;   fail
    ),
    !.
load_eps_or_default(0.20).

read_weights_csv(File, Weights) :-
    csv_read_file(File, Rows, [separator(0',), convert(false), strip(true)]),
    Rows = [Header|Data],
    header_len(Header, Arity),
    ( Arity =:= 2 -> true ; domain_error(two_columns_csv, File) ),
    maplist(row_to_kw, Data, Weights).

row_to_kw(row(K0,V0), K-V) :-
    to_atom(K0, K),
    to_number(V0, V).

read_eps_csv(File, Eps) :-
    csv_read_file(File, Rows, [separator(0',), convert(false), strip(true)]),
    Rows = [Header|Data],
    header_len(Header, Arity),
    ( Arity =:= 1 -> Data = [row(E0)|_], to_number(E0, Eps)
    ; Arity =:= 2 -> Data = [row(Name,Val)|_], downcase_atom_safe(Name, NameL),
                     ( NameL == eps -> to_number(Val, Eps)
                     ; domain_error(eps_row, File)
                     )
    ).

header_len(Row, N) :- Row =.. [_|Cols], length(Cols, N).

to_atom(A, A) :- atom(A), !.
to_atom(S, A) :- string(S), atom_string(A, S), !.

to_number(N, N) :- number(N), !.
to_number(A, N) :- atom(A),  catch(atom_number(A, N),  _, fail), !.
to_number(S, N) :- string(S), catch(number_string(N, S), _, fail), !.

downcase_atom_safe(A, L) :- atom(A), downcase_atom(A, L), !.
downcase_atom_safe(S, L) :- string(S), string_lower(S, SL), atom_string(L, SL).
