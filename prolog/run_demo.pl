% prolog/run_demo.pl
:- initialization(run, main).

run :-
    % set CWD to project root (parent of this prolog/ dir)
    prolog_load_context(directory, PrologDir),
    file_directory_name(PrologDir, ProjectRoot),
    working_directory(_, ProjectRoot),

    % load modules
    ensure_loaded('prolog/coffee/coffee_data.pl'),
    ensure_loaded('prolog/coffee/utilitarian_mcda.pl'),

    % write one merged CSV for all rounds
    utilitarian_mcda:demo_csv('F:/Logic/outputs/utilitarian_scores.csv'),

    halt(0).
