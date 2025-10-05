% prolog/run_all_coffee.pl
:- use_module(library(filesex)).
:- initialization(run, main).

run :-
    % cd to project root
    prolog_load_context(directory, PrologDir),
    file_directory_name(PrologDir, ProjectRoot),
    working_directory(_, ProjectRoot),

    % load modules
    ensure_loaded('prolog/coffee/coffee_data.pl'),
    ensure_loaded('prolog/coffee/utilitarian_mcda.pl'),
    ensure_loaded('prolog/coffee/kantian_rules_coffee.pl'),
    ensure_loaded('prolog/coffee/meta_explainer.pl'),
    ensure_loaded('prolog/coffee/confidence_meta.pl'),
    ensure_loaded('prolog/coffee/policy_trace_text.pl'),
    ensure_loaded('prolog/coffee/ui_rationales.pl'),
    ensure_loaded('prolog/coffee/config_params.pl'),

    % params (from CSV if present)
    config_params:load_weights_or_default(W),
    config_params:load_eps_or_default(Eps),

    % outputs dir
    OutDir = 'F:/Logic/outputs',
    make_directory_path(OutDir),

    % generate all CSVs
    atom_concat(OutDir, '/utilitarian_scores.csv', P1),
    utilitarian_mcda:demo_csv(P1),

    atom_concat(OutDir, '/decision_meta_config.csv', P2),
    meta_explainer:meta_csv_with(P2, Eps, W),

    atom_concat(OutDir, '/decision_trace_config.csv', P3),
    meta_explainer:meta_trace_csv_with(P3, Eps, W),

    atom_concat(OutDir, '/policy_trace_text.csv', P4),
    policy_trace_text:human_readable_trace_csv_with(P4, Eps, W),

    atom_concat(OutDir, '/decision_confidence.csv', P5),
    confidence_meta:confidence_csv(P5, Eps, 100, 0.15),

    atom_concat(OutDir, '/ui_rationales.csv', P6),
    ui_rationales:rationales_csv(P6, Eps, 100, 0.15),

    halt(0).
