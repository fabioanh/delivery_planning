:- module(clp_solution, [pickup/1]).

:- use_module(core).
:- use_module(library(clpfd)).

valid_product_pickup([], _).

valid_product_pickup([RP/RQ|Request], P/Q) :-
  P = RP,
  Q #> RQ,
  valid_product_pickup(Request, P/Q).

valid_pickup([], _).

valid_pickup([Product|Inventory], Request) :-
  valid_product_pickup(Request, Product),
  valid_pickup(Inventory, Request).

pickup(Pickup) :-
  get_orders(Orders),
  get_depots(Depots),
  assign_orders(Depots, Orders, [], Load).

%% assign_orders(_, [], Load, Load).

%% assign_orders(Depots, [Order|Orders], LoadAcc, TotalLoad) :-
%%   Depots \= [],
%%   subset(Depots, Source),
%%   findall(Inventory, member())
%%   .

get_orders(Orders) :-
    findall(order(ID, Prods, Loc, DL), order(ID, Prods, Loc, DL), Orders).

get_depots(Depots) :-
    findall(depot(ID, Prods, Loc), depot(ID, Prods, Loc), Depots).


create_assoc_list(Es,Ts,Assoc) :-
    empty_assoc(EmptyAssoc),
    findall(assign(E,T),(member(E,Es),member(T,Ts)),AssignmentPairs),
    build_assoc_list(EmptyAssoc,AssignmentPairs,Assoc).

% build_assoc_list(+AssocAcc,+Pairs,-Assoc)
build_assoc_list(Assoc,[],Assoc).

build_assoc_list(AssocAcc,[Pair|Pairs],Assoc) :-
    put_assoc(Pair,AssocAcc,_Var,AssocAcc2),
    build_assoc_list(AssocAcc2,Pairs,Assoc).


valid_product_pckp([], Result, Result).

valid_product_pckp([RP/RQ|Request], P/Q, Result) :-
  P = RP,
  Q #>= RQ,
  Val is Q - RQ,
  valid_product_pckp(Request, P/Val, Result).

valid_pckp([], _, Result, Result).

valid_pckp([Product|Inventory], Request, Acc, Result) :-
  valid_product_pckp(Request, Product, UpdatedProd),
  append(Acc, [UpdatedProd], RAcc),
  valid_pckp(Inventory, Request, RAcc, Result).

pckp(Results) :-
  get_orders(Orders),
  get_depots(Depots),
  findall(Inventory, member(depot(_,Inventory, _), Depots), Inventories),
  findall(OrderProducts, member(order(_,OrderProducts, _, _), Orders), OrderProducts),
  %% valid_pckp(Inventories, OrderProducts, [], Results).
  Results #= OrderProducts,
  valid_pickup(Inventories, OrderProducts).

  %% depot(DID, Inventory, _),
  %% order(OID, Request, _, _),
  %% valid_pickup(Inventory, Request).







valid_prods_quan_helper([], _).

valid_prods_quan_helper([product_quantity(RP, _)| ReqProdsQuant], product_quantity(P, Q)) :-
  RP \= P,
  valid_prods_quan_helper(ReqProdsQuant, product_quantity(P, Q)).


valid_prods_quan_helper([product_quantity(RP, RQ)| ReqProdsQuant], product_quantity(P, Q)) :-
  RP = P,
  Q #> RQ,
  valid_prods_quan_helper(ReqProdsQuant, product_quantity(P, Q)).

valid_products_quantity([], _).

valid_products_quantity([SPQ|SourceProdsQuantity], RequestProdsQuantity) :-
  valid_prods_quan_helper(RequestProdsQuantity, SPQ),
  valid_products_quantity(SourceProdsQuantity, RequestProdsQuantity).

valid_quantity([], _).

valid_quantity([Inventory|Inventories], Request) :-
  get_products_quantity(Inventory, [], InventoryProductsQuantity),
  get_products_quantity(Request, [], RequestProductsQuantity),
  valid_products_quantity(InventoryProductsQuantity, RequestProductsQuantity),
  valid_quantity(Inventories, Request).


get_products_quantity([], Result, Result).

get_products_quantity([P/Q|RawProds], Acc, Result) :-
  append(Acc,[product_quantity(P, Q)], RAcc),
  get_products_quantity(RawProds, RAcc, Result).

assemble_depot_ids([], Result, Result).

assemble_depot_ids([depot(DID, _, _)|Depots], Acc, Result) :-
  append(Acc, [DID], RAcc),
  assemble_depot_ids(Depots, RAcc, Result).

assign_orders(_, [], Routes, Routes).

assign_orders(Depots, [order(OID, Request, _, _)|Orders], Acc, Routes) :-
  subset(Depots, Source),
  findall(Inventory, member(depot(_, Inventory, _), Source), Inventories),
  valid_quantity(Inventories, Request),
  assemble_depot_ids(Source, [], Dpts),
  append(Acc, [order_depot(OID, Dpts)], RAcc),
  assign_orders(Depots, Orders, RAcc, Routes).

solve(Result) :-
  get_depots(Depots),
  get_orders(Orders),
  assign_orders(Depots, Orders, [], Result).


% subset(+List,-Subset)
%
% Returns all ordered subsets of the given list.
subset([], []).

subset([E|Tail], [E|NTail]):-
  subset(Tail, NTail).

subset([_|Tail], NTail):-
  subset(Tail, NTail).