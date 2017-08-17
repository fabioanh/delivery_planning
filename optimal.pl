%% Based on the resource found in: https://github.com/endymion64/Prolog-DAG-Scheduler/blob/master/src/find_optimal.pl

:- module(optimal, [optimal/1]).
:- use_module(solution, [is_valid/1]).
:- use_module(profit).
:- dynamic best/2.

%% find_optimal(-S)
%% Generates the optimal schedule maximizing the profit
find_optimal(_) :- 
    current_prolog_flag(min_tagged_integer, Init),
    assert(best(nil, Init)), 
    is_valid(X), 
    profit(X, ValueX), 
    update_best(X, ValueX),
    fail.

find_optimal(X) :- 
    best(X, _),
    retract(best(_, _)).

update_best(X, ValueX) :- 
    best(_, ValueBest), 
    ValueX > ValueBest, 
    !, 
    retract(best(_,_)),
    assert(best(X,ValueX)).

update_best(_, _).