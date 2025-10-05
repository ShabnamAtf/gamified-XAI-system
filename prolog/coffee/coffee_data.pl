% prolog/coffee/coffee_data.pl
:- module(coffee_data,
  [ load_coffee_data/0,
    coffee_opt/1,
    round_options/2,
    find_option/2
  ]).

:- use_module(library(csv)).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- dynamic coffee_opt/1.

coffee_csv('data/coffee/coffee_scenarios.csv').

load_coffee_data :-
    retractall(coffee_opt(_)),
    coffee_csv(File),
    csv_read_file(File, Rows, [separator(0',), convert(false), strip(true)]),
    Rows = [_Header|DataRows],
    maplist(row_to_dict, DataRows, Dicts),
    forall(member(D, Dicts), assertz(D)),
    !.

row_to_dict(Row, coffee_opt(D)) :-
    Row = row(RoundIdx,RoundName,OptionLabel,OptionID,Category,ProductName,Desc,
              Price,Carbon,Water,Transp,FarmerShare,DefRisk,ShadeCert,ChildRisk,
              Certs,PackRec,PackType,Taste,Freshness,BrewTime,Decaf,Vegan),
    to_num(RoundIdx, RIdxN),
    to_num(Price, PriceN), to_num(Carbon,CarbonN), to_num(Water,WaterN),
    to_num(Transp,TranspN), to_num(FarmerShare,FarmerShareN), to_num(DefRisk,DefRiskN),
    to_bool(ShadeCert, ShadeB), to_num(ChildRisk,ChildRiskN),
    to_cert_list(Certs, CertList), to_bool01(PackRec, PackRecB),
    to_num(Taste,TasteN), to_num(Freshness,FreshN), to_num(BrewTime,BrewN),
    to_bool(Vegan, VeganB),
    D = _{
        round_idx:RIdxN, round_name:RoundName, option_label:OptionLabel,
        option_id:OptionID, category:Category, product_name:ProductName,
        description:Desc,
        price_cad_per_cup:PriceN, carbon_g_co2e_per_cup:CarbonN,
        water_l_per_cup:WaterN, transparency_score:TranspN,
        farmer_income_share_pct:FarmerShareN, deforestation_risk:DefRiskN,
        shade_cert:ShadeB, child_labor_risk:ChildRiskN,
        certs:CertList, packaging_recyclability:PackRecB,
        packaging_type:PackType, taste_score_sca:TasteN,
        freshness_days_since_roast:FreshN, brew_time_min:BrewN,
        decaf_process:Decaf, vegan_cert:VeganB
    }.

to_num(S, N) :-
    ( number(S) -> N=S
    ; atom(S),    catch(atom_number(S,N),_,fail)
    ; string(S),  catch(number_string(N,S),_,fail)
    ), !.
to_num(S, N) :- atom_string(A,S), atom_number(A,N).

to_bool(true,  true)  :- !.
to_bool(false, false) :- !.
to_bool('True',  true)  :- !.
to_bool('False', false) :- !.
to_bool("True",  true)  :- !.
to_bool("False", false) :- !.
to_bool(1, true)  :- !.
to_bool(0, false) :- !.

to_bool01(1, 1) :- !.
to_bool01('1', 1) :- !.
to_bool01("1", 1) :- !.
to_bool01(true, 1) :- !.
to_bool01(_, 0).

to_cert_list(S, []) :- (S==""; S==''; S==""""), !.
to_cert_list(S, L) :-
    ( string(S) -> SS=S ; atom(S) -> atom_string(S,SS) ; SS="" ),
    atomic_list_concat(Toks, ';', SS),
    maplist([X,Y]>>(atom_string(A,X), downcase_atom(A,Y)), Toks, L).

round_options(RoundIdx, Opts) :-
    findall(D, (coffee_opt(D), D.round_idx =:= RoundIdx), Opts).

find_option(OptionID, D) :-
    coffee_opt(D), D.option_id == OptionID.
