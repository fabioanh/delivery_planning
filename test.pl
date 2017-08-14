% manhattan_distance(+FromID, +ToID, -Distance)
%% Gives the manhattan distance in kilometers between the input ID locations
manhattan_distance(FromID, ToID, Distance) :-
  get_location(FromID, location(X1, Y1)),
  get_location(ToID, location(X2, Y2)),
  XDis is abs(X1 - X2),
  YDis is abs(Y1 - Y2),
  Distance is float(XDis + YDis).

% get_location(+ResourceID, -location(X, Y))
%% Finds the location of either depots or packages given the identifier of the resource.
get_location(ResourceID, location(X, Y)) :-
  depot(ResourceID, _, location(X,Y)), !.

get_location(ResourceID, location(X, Y)) :-
  order(ResourceID, _, location(X,Y), _), !.

% decimal_round(+Val, +DecimalPositions, -Rounded)
%% Rounds a float value to the desired number of decimal positions
decimal_round(Val, DecimalPositions, Rounded) :-
  RoundFactor is (10 ** DecimalPositions),
  Tmp is round(Val * RoundFactor),
  Rounded is Tmp / RoundFactor.

% driving_duration(+VID,+FromID,+ToID,-Duration).
%% Gives the duration in minutes of a vehicle VID.
%% Time spent going from the depot/oder FromID to the depot/order ToID
driving_duration(VID, FromID, ToID, Duration) :-
  manhattan_distance(FromID, ToID, Distance),
  vehicle(VID, _, _, Pace, _, _),
  decimal_round(Distance * Pace, 1, Duration).




% order_value(+ProductsDetailsList, +Value, -Result)
%% Recursive function used to go through a list of order details, 
%% get the products information and compute the total order value.
order_value([], Value, Value).

order_value([ProductDetails|ProductsDetails], Value, Result) :-
  ProductID/Quantity = ProductDetails,
  product(ProductID, Val, _),
  R is Value + (Val * Quantity),
  order_value(ProductsDetails, R,  Result).

% discount_factor(+Day, +Deadline, -Factor)
%% Check if the given Day is before the Deadline and assign the proper discount factor 
%% when this condition is not met.
discount_factor(Day, Deadline, Factor) :-
  Day =< Deadline,
  Factor is 1.

discount_factor(Day, Deadline, Factor) :-
  Day > Deadline,
  Factor is float(0.5).

% earning(+OID,+Day,-Value)
%% Computes the revenue received for delivering order OID on day Day
earning(OID, Day, Value) :-
  working_day(Day, _, _),
  order(OID, ProductList, _, Deadline),
  order_value(ProductList, 0, OrderValue),
  discount_factor(Day, Deadline, DiscountFactor),
  decimal_round(OrderValue * DiscountFactor, 1, Value).




% order_weight(+ProductsDetailsList, +Acc, -Weight)
%% Computes the weight of an order based on its product details: Weight and Quantity
%% Give a set of Product Details in the form [p1ID/quantity1, ..., pkID,quantityK] and Acc = 0
%% to compute the weight of the products identified by the ids in the list
order_weight([], Weight, Weight).

order_weight([ProductDetails|ProductsDetails], Acc, Weight) :-
  ProductID/Quantity = ProductDetails,
  product(ProductID, _, ProductWeight),
  decimal_round(Acc + (ProductWeight * Quantity), 1, R),
  order_weight(ProductsDetails, R, Weight).

% load_weight(+Orders, +Acc, -Weight).
%% Auxiliary recursive function to compute the weight of load composed by a list of orders.
%% Give a list of order IDs [o1, o2, ..., oK] and Acc = 0 in order to obtain the total weight 
%% of the listed orders.
load_weight([], Weight, Weight).

load_weight([Order|Orders], Acc, Weight) :-
  order(Order, ProductsDetails, _, _),
  order_weight(ProductsDetails, 0, OrderWeight),
  decimal_round(Acc + OrderWeight, 1, R),
  load_weight(Orders, R, Weight).

% load(+Orders, -Weight)
%% Gives the weight of a list of orders.
load(Orders, Weight) :-
  load_weight(Orders, 0, Weight).




% subtract_value(+Product/Quantity, +ProductsDetailsList, -Result)
%% Looking for a product matching the Product input in the ProductDetails list
%% and subtracts its quantity value to the input Quanity for the given Product
subtract_value(P/Q, [], P/Q).

subtract_value(Product/Quantity, [Product/Q|_], ResProd/ResQuan) :-
  Quan is Quantity - Q,
  ResProd/ResQuan = Product/Quan,
  !. % Check how to remove this cut. A bit dirty.

subtract_value(Product/Quantity, [_|Products], Result) :-
  subtract_value(Product/Quantity, Products, Result).

% subtract_products_list(+ProductsList, +SubtractionList, +Acc, -Result)
%% Subtract a list containing product/quantity information from another list
%% with the same kind of data. Returns a Result with a new list containing the
%% product/quantity information updated => Products - SubtractionList
subtract_products_list([], _, Result, Result).

subtract_products_list([Product|Products], SubtractionList, Acc, Result) :-
  subtract_value(Product, SubtractionList, SubtractedProduct),
  append(Acc, [SubtractedProduct], AccRes),
  subtract_products_list(Products, SubtractionList, AccRes, Result).

positive_quantities([]).
positive_quantities([_/Q|Prods]) :-
  Q >= 0,
  positive_quantities(Prods).

% update_inventory(+Inventory, ?OrderID, ?NewInventory)
%% Updates Inventory according to the product details information of the given OrderID
%% subtracting the corresponding product quantities from the ones provided as
%% Inventory.
update_inventory(Inventory, OID, NewInventory) :-
  order(OID, Products, _, _),
  subtract_products_list(Inventory, Products, [], ResultInventory),
  positive_quantities(ResultInventory),
  NewInventory = ResultInventory. 
  % Validate if the first value is enough. If so add a cut.
  %If multiple values are required leave without cut.











%% schedule(v2,1,[o6,d2,o8,d1]).

%% loop through the Route, add the driving_duration between every two consecutive points
%% and subtract it from the TimeLeft
is_route_on_time(_, [], _).

is_route_on_time(_, [_], _).

is_route_on_time(Pace, [Current, Next | Rest], TimeLeft) :- 
  manhattan_distance(Current, Next, Distance),
  TLeft is TimeLeft - (Pace * Distance),
  TLeft >= 0,
  is_route_on_time(Pace, [Next|Rest], TLeft).

% orders_overhead(+LocationsList, +AccumulatorExtraTime, +OrdersExtraTime)
%% Iterates over the locations in the route finding the amount of extra time needed to process all orders
orders_overhead([], OrdersExtraTime, OrdersExtraTime).

orders_overhead([Current|Next], Acc, OrdersExtraTime) :-
  depot(Current, _, _),
  orders_overhead(Next, Acc, OrdersExtraTime).

orders_overhead([Current|Next], Acc, OrdersExtraTime) :-
  order(Current, _, _, _),
  ET is Acc + 10,
  orders_overhead(Next, ET, OrdersExtraTime).

% is_last_depot(+LocationsList)
%% Checks whether the last element of the input list of locations is a Depot
is_last_depot([]).

is_last_depot([DID]) :-
  depot(DID, _, _).

is_last_depot([_|T]) :-
  T \= [],
  is_last_depot(T).

% is_right_load(+Capacity, +RouteList, +TotalLoadValue).
%% Iterates over the list of locations (route) checking if the 
%% commulative Total Load Value is lower than the Capacity 
is_right_load(_, [], _).

is_right_load(_, [DID], _) :-
  depot(DID, _, _), !.

is_right_load(Capacity, [DID|Route], Acc) :-
  depot(DID, _, _),
  is_right_load(Capacity, Route, Acc).

is_right_load(Capacity, [OID|Route], Acc) :-
  load([OID], OrderWeight), 
  RAcc is Acc + OrderWeight,
  Capacity - RAcc >= 0,
  is_right_load(Capacity, Route, RAcc).


% A schedule is valid if:
% - The Day is a working_day
% - Vehicle load is never higher than its maximum capacity
% - Following the route for the vehicle VID takes less time than what it has in the working day
% - The end of the route is a Deposit
% - Before going to a depot the vehicle should be always empty
is_schedule_valid(schedule(VID, Day, [])) :-
  working_day(Day, _, _),
  vehicle(VID, _, _, _, _, _).
  %% !.

is_schedule_valid(schedule(VID, Day, Route), Origin) :-
  vehicle(VID, _, Capacity, Pace, _, _),
  working_day(Day, StartTime, EndTime),
  orders_overhead(Route, 0, OrdersExtraTime),
  TimeLeft is EndTime - StartTime - OrdersExtraTime,
  is_route_on_time(Pace, [Origin|Route], TimeLeft),
  is_last_depot(Route),
  is_right_load(Capacity, Route, 0).

% Inventory validation:
% - Find all vehicles
% - For each vehicle get all schedules for a vehicle
% - Sort all those schedules
% - Join all routes from those schedules + the initial depot for vehicle
% - Separate groups of orders between every two depots: Go through list.
%     - If depot create new inventory(DID, OrdersList)
%     - If order loop and attach it to the list of [orders] until Depot found
%     - If empty, Return the list of inventories.
% - Join the inventories for the depots

get_routes([], Result, Result).

get_routes([schedule(_,_,Route)|Schedules], Acc, Result) :-
  append(Acc, Route, RAcc),
  get_routes(Schedules, RAcc, Result).

% schedule_belongs_to_vehicle(+VehicleID, +Schedule)
%% Condition used to filter values in a list. Must evaluate to true when the VehicleID matches the value of
%% the vehicle identifier in a schedule data structure.
schedule_belongs_to_vehicle(VID, schedule(VID, _, _)).

% get_vehicle_routes(+VehiclesList, +SchedulesList, +Accumulator, -VehicleRoutesList)
%% Iterates over the list of vehicles getting the list of Routes (list of locations) for each vehicle
%% in the input list of Schedules. Takes advantage of the fact that the list of Schedules is ordered
%% by Working Day, so that the first element in the total route is the original location (depot) 
%% of the vehicle
get_vehicle_routes([], _, VehicleRoutes, VehicleRoutes).

get_vehicle_routes([Vehicle|Vehicles], Schedules, Acc, VehicleRoutes) :-
  vehicle(Vehicle, DID, _, _, _, _),
  include(schedule_belongs_to_vehicle(Vehicle), Schedules, VSchedules),
  sort(VSchedules, SortedVSchedules),
  get_routes(SortedVSchedules, [DID], FullRoutes),
  append(Acc, [FullRoutes], RAcc),
  get_vehicle_routes(Vehicles, Schedules, RAcc, VehicleRoutes).

% orders_from_route(+DepotID, +LocationsList, +Ignore, +Accumulator, -ResultList)
%% Filters only the Orders list related to the given Depot ID
%% for the list of Locations (route) given as input
orders_from_route(_, [], _, Result, Result).

orders_from_route(DID, [Location|Locations], Ignore, Acc, Result) :-
  order(Location, _, _, _),
  Ignore,
  orders_from_route(DID, Locations, Ignore, Acc, Result).

orders_from_route(DID, [Location|Locations], Ignore, Acc, Result) :-
  order(Location, _, _, _),
  \+ Ignore,
  append(Acc, [Location], RAcc),
  orders_from_route(DID, Locations, Ignore, RAcc, Result).

orders_from_route(DID, [Location|Locations], _, Acc, Result) :-
  depot(Location, _, _),
  Location \= DID,
  orders_from_route(DID, Locations, true, Acc, Result).

orders_from_route(DID, [Location|Locations], _, Acc, Result) :-
  depot(Location, _, _),
  Location = DID,
  orders_from_route(DID, Locations, false, Acc, Result).

% extract_orders(+DepotID, +RoutesList, +Accumulator, +OrdersList)
%% Iterates over the input Routes List (a list of lists of locations [[d1, o1, ...],[d2, o3, ...], ...])
%% in order to obtain only the orders from these routes and compute
extract_orders(_, [], Result, Result).

extract_orders(Depot, [Route|Routes], Acc, Result) :-
  orders_from_route(Depot, Route, true, [], Orders),
  append(Acc, Orders, RAcc),
  extract_orders(Depot, Routes, RAcc, Result).

% orders_by_depot(+DepotsList, +VehicleRoutesList, +Accumulator, -Result)
%% Iterates on the list of Depots provided as input
%% to obtain the list of depot_orders(DepotID, [o1, o2, ...])
%% related to it based on the routes for the vehicle given as input.
orders_by_depot([], _, Result, Result).

orders_by_depot([Depot|Depots], VehicleRoutes, Acc, Result) :-
  extract_orders(Depot, VehicleRoutes, [], Orders),
  append(Acc, [depot_orders(Depot, Orders)], RAcc),
  orders_by_depot(Depots, VehicleRoutes, RAcc, Result).

% valid_inventory(+OrdersList, +CurrentInventory)
%% Checks whether the List of Orders can take elements from the Depot's inventory without taking more than
%% what is available.
valid_inventory([], _).

valid_inventory([Order|Orders], CurrentInventory) :-
  update_inventory(CurrentInventory, Order, NewInventory),
  valid_inventory(Orders, NewInventory).

% valid_orders_by_depot(+DepotOrdersList)
%% Using the structure depot_orders(DepotID, OrdersList), where the OrdersList is the complete list of
%% orders for a given depot in the solution, checks whether the orders can take items from the depots
%% without surpassing the maximum value contained in the Depots' inventory.
valid_orders_by_depot([]).

valid_orders_by_depot([depot_orders(Depot, Orders)|DepotOrders]) :- 
  depot(Depot, Inventory, _), 
  valid_inventory(Orders, Inventory),
  valid_orders_by_depot(DepotOrders).

% is_inventory_valid(+SchedulesList)
%% Checks whether the Load of orders at depot (Inventory) has positive values for the orders in a route.
%% Depots are not re-stocked.
%% Taking advantage of the constructed Orders by Depot (depot_orders(DepotID, OrdersList)) checks whether
%% the list of orders contains unique values calling the only_once_orders predicate.
is_inventory_valid(Schedules) :-
  findall(X, (vehicle(X, _, _, _, _, _)), Vehicles),
  get_vehicle_routes(Vehicles, Schedules, [], VehicleRoutes),
  findall(X, (depot(X, _, _)), Depots),
  orders_by_depot(Depots, VehicleRoutes, [], OrdersByDepot),
  valid_orders_by_depot(OrdersByDepot),
  only_once_orders(OrdersByDepot).

% are_schedules_valid(+SchedulesList, +Origin)
%% Verifies that the Schedules List is valid according to the constraints listed for the predicate 
%% 'schedules_valid'.
%% The current method is in charge of the following logic:
%% - Get schedules by vehicle
%% - Sort schedules (Because of natural order they get sorted by day)
%% - Process (Validates) each schedule setting as origin the last location of previous schedule

are_schedules_valid([], _).

are_schedules_valid([Schedule|Schedules], Origin) :-
  is_schedule_valid(Schedule, Origin),
  schedule(_, _, Route) = Schedule,
  Route = [],
  are_schedules_valid(Schedules, Origin).


are_schedules_valid([Schedule|Schedules], Origin) :-
  is_schedule_valid(Schedule, Origin),
  schedule(_, _, Route) = Schedule,
  Route \= [],
  last(Route, NewOrigin),
  are_schedules_valid(Schedules, NewOrigin).

% schedules_valid_by_vehicles(+SchedulesList, +VehiclesList)
%% Iterates over the list of vehicles to validate the schedules gropued by each vehicle
schedules_valid_by_vehicles(_, []).

schedules_valid_by_vehicles(Schedules, [VID|Vehicles]) :-
  include(schedule_belongs_to_vehicle(VID), Schedules, VSchedules),
  sort(VSchedules, SortedVSchedules),
  vehicle(VID, Origin, _, _, _, _),
  are_schedules_valid(SortedVSchedules, Origin),
  schedules_valid_by_vehicles(Schedules, Vehicles).

%schedules_valid(+SchedulesList)
%% Checks whether the routes in the list of schedules can be delivered in the right time window for the
%% working days. Validates also that the load for the vehicle is not higher than its maximum allowed value.
%% And checks that the vehicles end their daily routes always in a depot.
schedules_valid(Schedules) :-
  findall(X, (vehicle(X, _, _, _, _, _)), Vehicles),
  schedules_valid_by_vehicles(Schedules, Vehicles).

% get_all_orders(+DepotOrders, +Accumulator, -Result)
%% Returns the whole list of Orders from the input list of [depot_orders(DepotID, OrdersList), ...]
get_all_orders([], Result, Result).

get_all_orders([depot_orders(_, Orders)|DepotOrders], Acc, Result) :-
  append(Acc, Orders, RAcc),
  get_all_orders(DepotOrders, RAcc, Result).

% contains_duplicates(+List)
%% Checks whether the given list contains or not duplicate values.
contains_duplicates(List) :-
  length(List, InitialLength),
  sort(List, SortedList),
  length(SortedList, FinalLength),
  InitialLength \= FinalLength.

only_once_orders(DepotOrders) :-
  get_all_orders(DepotOrders, [], AllOrders),
  \+ contains_duplicates(AllOrders).

% valid_complete_schedules(+SchedulesList, +VehicleID, +WorkingDayID)
%% Checks that the Schedules list contains at least one value for the input Vehicle and Working Day
valid_complete_schedules(Schedules, VID, WDID) :-
  member(schedule(VID, WDID, _), Schedules), !.

% valid_complete_vehicles(+VehiclesList, +SchedulesList, +WorkingDay)
%% Iterate over the list of Vehicles checking if the Schedules are valid for the given Working Day
valid_complete_vehicles([], _, _).

valid_complete_vehicles([Vehicle|Vehicles], Schedules, WorkingDay) :-
  valid_complete_schedules(Schedules, Vehicle, WorkingDay),
  valid_complete_vehicles(Vehicles, Schedules, WorkingDay).

% valid_complete_working_days(+WorkingDaysList, +VehiclesList, +SchedulesList)
%% Iterate over the WorkingDays to check if there are values for all Vehicles in the input SchedulesList
valid_complete_working_days([], _, _).

valid_complete_working_days([WorkingDay|WorkingDays], Vehicles, Schedules) :-
  valid_complete_vehicles(Vehicles, Schedules, WorkingDay),
  valid_complete_working_days(WorkingDays, Vehicles, Schedules).

% complete_working_days(+Schedules)
%% - In a plan, for every working day there should be a schedule plan for all vehicles even if it is empty
complete_working_days(Schedules) :-
  findall(X, (working_day(X, _, _)), WorkingDays),
  findall(X, (vehicle(X, _, _, _, _, _)), Vehicles),
  valid_complete_working_days(WorkingDays, Vehicles, Schedules).

% is_valid(+Plan)
%% Validates whether a plan is valid or not checking the hard constraints for the problem.
is_valid(plan(Schedules)) :-
  complete_working_days(Schedules),
  schedules_valid(Schedules),
  is_inventory_valid(Schedules).