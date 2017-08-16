/*  File:    load.pl
    Purpose: Load my program
*/
%% :- dynamic depot/3.
%% :- [ 'instances/multi_small.pl'].
%% :-  use_module(core).
%% :-  use_module(print).
%% :-  use_module(valid).
%% :-  use_module(profit).


:- module(main,[]).
:- reexport([ core,
              print,
              valid,
              profit,
              clp_solution
            ]).
