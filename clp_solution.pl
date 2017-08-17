%% Implementation of a Constraint Logic Programming over a Finite Domain solution.
%% Inspiration taken from https://github.com/mjones-credera/prolog-samples where
%% examples are given together with an explanation on the solution found in
%% https://www.credera.com/blog/technology-insights/open-source-technology-insights/solving-logical-business-problems-using-prolog-part-3/ 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Solution not implemented successfuly due to lack of knowledge on how to model the problem over a finite Domain
%% and due to the lack of correct understanding of how to make the best use of the methods to construct
%% the solution in such a way that the optimization could be performed. In concrete, lack of understanding
%% of how to use serialize in order to generate sequential tasks that would correspond to the vehicle operations.
%% Also missing the correct concept on how to use the maximization methods (although that would be needed close to the end.)
%% And the most important part is the lack of a background to recognize how to transform the constraint into a
%% finite domain representation to be able to use the logical statements provided by the library.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(clp_solution, [get_order_depots/1]).

:- use_module(core).
:- use_module(library(clpfd)).


% valid_prods_quan_helper(+ProductQuantityList, +ProductQuantity) :-
%% Helper function to identify whether a matching value for the product information, of a list
%% of product_quantity structures, is lower than the provided Q (quantity)
%% matching its own quantity RQ
valid_prods_quan_helper([], _).

valid_prods_quan_helper([product_quantity(RP, _)| ReqProdsQuant], product_quantity(P, Q)) :-
  RP \= P,
  valid_prods_quan_helper(ReqProdsQuant, product_quantity(P, Q)).


valid_prods_quan_helper([product_quantity(RP, RQ)| ReqProdsQuant], product_quantity(P, Q)) :-
  RP = P,
  Q #> RQ,
  valid_prods_quan_helper(ReqProdsQuant, product_quantity(P, Q)).


%valid_products_quantity(+SourceProdsQuantity, +RequestProdsQuantity) :-
%% Verifies whether the list of source products quantity can fulfill
%% (has enough quantities for products) for the list of request products quantity
%% SourceProdsQuantity > RequestProdsQuantity ?
valid_products_quantity([], _).

valid_products_quantity([SPQ|SourceProdsQuantity], RequestProdsQuantity) :-
  valid_prods_quan_helper(RequestProdsQuantity, SPQ),
  valid_products_quantity(SourceProdsQuantity, RequestProdsQuantity).

% get_feasible_depots(+DepotsList, +Order, +Accumulator, -FeasibleDepotIDsList).
%% Gets a list of Depots' identifiers for the depots that can fulfill the
%% products request of the given Order
get_feasible_depots([], _, FeasibleDepots, FeasibleDepots).

get_feasible_depots([depot(_, Inventory, _)|Depots], order(Order, Request, _, _), Acc, FeasibleDepots) :-
  get_products_quantity(Inventory, [], InventoryProductsQuantity),
  get_products_quantity(Request, [], RequestProductsQuantity),
  \+ valid_products_quantity(InventoryProductsQuantity, RequestProductsQuantity),
  get_feasible_depots(Depots, order(Order, Request, _, _), Acc, FeasibleDepots).

get_feasible_depots([depot(Depot, Inventory, _)|Depots], order(Order, Request, _, _), Acc, FeasibleDepots) :-
  get_products_quantity(Inventory, [], InventoryProductsQuantity),
  get_products_quantity(Request, [], RequestProductsQuantity),
  valid_products_quantity(InventoryProductsQuantity, RequestProductsQuantity),
  append(Acc, [Depot], RAcc),
  get_feasible_depots(Depots, order(Order, Request, _, _), RAcc, FeasibleDepots).

% get_order_depots(+OrdersList, +DepotsList, +Accumulator, -OrderDepots)
%% Iterates over the List of orders getting the possible depots that can fulfill its requirements
get_order_depots([], _, OrderDepots, OrderDepots).

get_order_depots([Order|Orders], Depots, Acc, OrderDepots) :-
  get_feasible_depots(Depots, Order, [], FeasibleDepots),
  append(Acc, [order_depot(Order, FeasibleDepots)], RAcc),
  get_order_depots(Orders, Depots, RAcc, OrderDepots).

% get_order_depots(-OrderDepots)
%% Gets a list: [order_depot(OrderID1, [DepotID1, DepotID2, ....]), order_depot(OrderID2, [...])]
%% where the identifiers for the Depots are the possible depots that can provide
%% the items to fulfill the request products of the related Order 
get_order_depots(OrderDepots) :-
  get_orders(Orders),
  get_depots(Depots),
  get_order_depots(Orders, Depots, [], OrderDepots).

%% get_routes()



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Code under construction / testing
%% |||||||||||||||||||||||||||||||||
%% vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
  assign_orders(Depots, Orders, [], Pickup).

%% assign_orders(_, [], Load, Load).

%% assign_orders(Depots, [Order|Orders], LoadAcc, TotalLoad) :-
%%   Depots \= [],
%%   subset(Depots, Source),
%%   findall(Inventory, member())
%%   .

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









valid_quantity([], _).

valid_quantity([Inventory|Inventories], Request) :-
  get_products_quantity(Inventory, [], InventoryProductsQuantity),
  get_products_quantity(Request, [], RequestProductsQuantity),
  valid_products_quantity(InventoryProductsQuantity, RequestProductsQuantity),
  valid_quantity(Inventories, Request).




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







