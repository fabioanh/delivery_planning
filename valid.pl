:- module(valid, [is_valid/1]).

:- use_module(core).

%% schedule(v2,1,[o6,d2,o8,d1]).

%% loop through the Route, add the driving_duration between every two consecutive points
%% and subtract it from the TimeLeft
is_route_on_time(_, [], _).

is_route_on_time(_, [X], _) :-
  get_location(X, _).

is_route_on_time(Pace, [Current, Next | Rest], TimeLeft) :-
  Current \= Next,
  manhattan_distance(Current, Next, Distance),
  TLeft is TimeLeft - (Pace * Distance),
  TLeft >= 0,
  is_route_on_time(Pace, [Next|Rest], TLeft).

% orders_overhead(+LocationsList, +AccumulatorExtraTime, +OrdersExtraTime)
%% Iterates over the locations in the route finding the amount of extra time needed to process all orders
orders_overhead([], OrdersExtraTime, OrdersExtraTime).

orders_overhead([Current|Next], Acc, OrdersExtraTime) :-
  Current \= Next,
  depot(Current, _, _),
  orders_overhead(Next, Acc, OrdersExtraTime).

orders_overhead([Current|Next], Acc, OrdersExtraTime) :-
  Current \= Next,
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

is_route_coherent([]).

is_route_coherent([X]) :-
  get_location(X, _).

is_route_coherent([Current, Next|Locations]) :-
  get_location(Current, _),
  get_location(Next, _),
  Current \= Next,
  is_route_coherent([Next|Locations]).


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
  is_route_coherent([Origin|Route]),
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

% get_vehicles_routes(+VehiclesList, +SchedulesList, +Accumulator, -VehicleRoutesList)
%% Iterates over the list of vehicles getting the list of Routes (list of locations) for each vehicle
%% in the input list of Schedules. Takes advantage of the fact that the list of Schedules is ordered
%% by Working Day, so that the first element in the total route is the original location (depot)
%% of the vehicle
get_vehicles_routes([], _, VehicleRoutes, VehicleRoutes).

get_vehicles_routes([Vehicle|Vehicles], Schedules, Acc, VehicleRoutes) :-
  get_single_vehicle_route(Vehicle, Schedules, FullRoute),
  append(Acc, [FullRoute], RAcc),
  get_vehicles_routes(Vehicles, Schedules, RAcc, VehicleRoutes).

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
  get_vehicles_routes(Vehicles, Schedules, [], VehicleRoutes),
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
  \+ contains_duplicates(SortedVSchedules),
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

only_once_orders(DepotOrders) :-
  get_all_orders(DepotOrders, [], AllOrders),
  \+ contains_duplicates(AllOrders).

% valid_complete_schedules(+SchedulesList, +VehicleID, +WorkingDayID)
%% Checks that the Schedules list contains at least one value for the input Vehicle and Working Day
valid_complete_schedules(Schedules, VID, WDID) :-
  select(schedule(VID, WDID, _), Schedules, Rest),
  \+ member(schedule(VID, WDID, _), Rest), !.
  %% member(schedule(VID, WDID, _), Schedules), !.

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
  schedules_valid(Schedules),
  is_inventory_valid(Schedules),
  complete_working_days(Schedules).




% *** FAILED ATTEMPT TO RE-WRITE ROUTE VALIDATION
% A route is valid if:
%   - All elements are valid locations
%   - No consecutive depots
%   - Route is on time
%   - Last element is a depot
%   - 
%% is_route_valid([]).

%% is_route_valid([X]) :-
%%   get_location(X, _).

%% is_route_valid(_, _, [Current, Next|[]]) :-
%%   get_location(Current, _),
%%   depot(Next, _, _),
%%   Current \= Next.

%% is_route_valid(VID, Origin, [Current, Next|Tail]) :-
%%   get_location(Current, X),
%%   get_location(Next, Y),
%%   X \= Y,
%%   vehicle(VID, _, _, Pace, _, _).
  %% is_route_on_time(Pace, )
