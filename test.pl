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

get_route([], Result, Result).

get_route([schedule(_,_,Route)|Schedules], Acc, Result) :-
  append(Acc, Route, RAcc),
  get_route(Schedules, RAcc, Result).

% get_single_vehicle_route(+VehicleID, +SchedulesList, -Route)
%% Given a Vehicle ID gets the entire route for this vehicle in the given Schedules
get_single_vehicle_route(VID, Schedules, Route) :-
  vehicle(VID, DID, _, _, _, _),
  include(schedule_belongs_to_vehicle(VID), Schedules, VSchedules),
  sort(VSchedules, SortedVSchedules),
  get_route(SortedVSchedules, [DID], Route).

get_single_vehicle_route(VID, DepotID, Schedules, Route) :-
  vehicle(VID, _, _, _, _, _),
  include(schedule_belongs_to_vehicle(VID), Schedules, VSchedules),
  sort(VSchedules, SortedVSchedules),
  get_route(SortedVSchedules, [DepotID], Route).

% schedule_belongs_to_vehicle(+VehicleID, +Schedule)
%% Condition used to filter values in a list. Must evaluate to true when the VehicleID matches the value of
%% the vehicle identifier in a schedule data structure.
schedule_belongs_to_vehicle(VID, schedule(VID, _, _)).

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











% BELONGS TO BASIC FUNCTIONALITIES MODULE
% driving_cost(+VehicleID, +FromID, +ToID, -Cost)
%% Obtains the cost of driving a vehicle from one location to another
driving_cost(VID, FromID, ToID, Cost) :-
  manhattan_distance(FromID, ToID, Distance),
  vehicle(VID, _, _, _, _, KmCost),
  decimal_round(Distance * KmCost, 1, Cost).

% route_cost(+VehicleID, +FromID, +LocationsList, +Accumulator, -Result)
%% Computes the cost of a route identified as a list of locations for a given Vehicle
route_cost(_, _, [], Cost, Cost).

route_cost(VID, FromID, [ToID|Locations], Acc, Cost) :-
  driving_cost(VID, FromID, ToID, TmpCost),
  decimal_round(Acc + TmpCost, 1, RAcc),
  route_cost(VID, ToID, Locations, RAcc, Cost).

% route_cost(+VehicleID, +LocationsList, -Cost)
%% Auxiliary function to validate the case when the list of locations is empty.
%% If the list is not empty calls route_cost/5
route_cost(VID, Route, Cost) :-
  Route \= [],
  [Origin|Locations] = Route,
  route_cost(VID, Origin, Locations, 0, Cost).

route_cost(_, Route, Cost) :-
  Route = [],
  Cost is 0.

% expenses_vehicles_routes(+VehiclesList, +SchedulesList, +Accumulator, -Cost)
%% Iterates over a list of vehicles, using the input list of schedules computes the
%% cost of the Route for each vehicle and sums all these values into a total cost.
expenses_vehicles_routes([], _, Cost, Cost).

expenses_vehicles_routes([VID|Vehicles], Schedules, Acc, Result) :-
  get_single_vehicle_route(VID, Schedules, Route),
  route_cost(VID, Route, Cost),
  decimal_round(Acc + Cost, 1, RAcc),
  expenses_vehicles_routes(Vehicles, Schedules, RAcc, Result).

% driving_expenses(+Schedules, -Expenses)
%% Gives the driving expenses for a given schedule.
%% Wrapper of the expenses_vehicles_routes/4 predicate used to pass the list of
%% all vehicles in theh problem.
driving_expenses(Schedules, Expenses) :-
  findall(X, (vehicle(X, _, _, _, _, _)), Vehicles),
  expenses_vehicles_routes(Vehicles, Schedules, 0, Result),
  decimal_round(Result, 1, Expenses).

% vehicles_usage_cost(+SchedulesList, +Accumulator, +Result)
%% Computes the cost of using a vehicle for the given Schedules List
vehicles_usage_cost([], Cost, Cost).

vehicles_usage_cost([schedule(_, _, Route)|Schedules], Acc, Result) :-
  Route = [],
  vehicles_usage_cost(Schedules, Acc, Result).

vehicles_usage_cost([schedule(VID, _, Route)|Schedules], Acc, Result) :-
  Route \= [],
  vehicle(VID, _, _, _, Cost, _),
  decimal_round(Acc + Cost, 1, RAcc),
  vehicles_usage_cost(Schedules, RAcc, Result).

% Auxiliary function to filter Schedules that belong to a given Working Day id.
schedule_in_working_day(WDID, schedule(_, WDID, _)).

% expenses_vehicles_usage(+WorkingDaysList, +SchedulesList, +Accumulator, -Result).
%% Iterates over the working days getting the static cost of using a vehicle on
%% each working day
expenses_vehicles_usage([], _, Cost, Cost).

expenses_vehicles_usage([WDID|WorkingDays], Schedules, Acc, Result) :-
  include(schedule_in_working_day(WDID), Schedules, WDSchedules),
  vehicles_usage_cost(WDSchedules, 0, Cost),
  decimal_round(Acc + Cost, 1, RAcc),
  expenses_vehicles_usage(WorkingDays, Schedules, RAcc, Result).

% fixed_cost_expenses(+SchedulesList, -Expenses)
%% Wrapper function to compute the static cost of using the vehicles for a
%% list of schedules
fixed_cost_expenses(Schedules, Expenses) :-
  findall(X, (working_day(X, _, _)), WorkingDays),
  expenses_vehicles_usage(WorkingDays, Schedules, 0, Result),
  decimal_round(Result, 1, Expenses).

% expenses(+Schedules, -Expenses)
%% Sum of costs involved in a schedule
%% - Cost for driving the car a distance given by a route
%% - Fixed cost for using a vehicle on a working day
expenses(Schedules, Expenses) :-
  driving_expenses(Schedules, DrivingExpenses),
  fixed_cost_expenses(Schedules, FCExpenses),
  decimal_round(DrivingExpenses + FCExpenses, 1, Expenses).

% route_revenue(+LocationsList, +DayID, +Accumulator, -Revenue)
%% Iterates over the Locations List getting the revenue for the found orders and
%% adding it together to compute the final revenue
route_revenue([], _, Revenue, Revenue).

route_revenue([LocationID|Locations], WDID, Acc, Result) :-
  \+ order(LocationID, _, _, _),
  route_revenue(Locations, WDID, Acc, Result).

route_revenue([LocationID|Locations], WDID, Acc, Result) :-
  order(LocationID, _, _, _),
  earning(LocationID, WDID, Earning),
  decimal_round(Acc + Earning, 1, RAcc),
  route_revenue(Locations, WDID, RAcc, Result).

% schedules_revenue(+SchedulesList, Accumulator, Revenue)
%% Iterates over the schedules list getting the revenue for each of them
schedules_revenue([], Result, Result).

schedules_revenue([schedule(_, WDID, Route)|Schedules], Acc, Result) :-
  route_revenue(Route, WDID, 0, Revenue),
  decimal_round(Acc + Revenue, 1, RAcc),
  schedules_revenue(Schedules, RAcc, Result).

% revenue(+SchedulesList, -Revenue)
%% Wrapper predicate to retrieve the revenue for the given schedules
revenue(Schedules, Revenue) :-
  schedules_revenue(Schedules, 0, Revenue).

%% Calculates the Revenue - Expenses
profit(plan(Schedules), Profit) :-
  revenue(Schedules, Revenue),
  expenses(Schedules, Expenses),
  decimal_round(Revenue - Expenses, 1, Profit). % check if round is required













% tabbed_print(+Data)
%% Prints the 4 values contained in the list input Data with a given tabular format
tabbed_print(Data) :-
  %% format('~s~t~8| ~s~t~16| ~s~t~24| ~s~t~120|~n', Data).
  format('~s~t~8| ~s~t~20| ~s~t~28| ~s~t~n', Data).

% prepend_zero(+Input, -Output)
%% Function to prepend a zero to hour values lower than 10
prepend_zero(Str, Str) :-
  string_length(Str, Len),
  Len \= 1.

prepend_zero(Str, Output) :-
  string_length(Str, Len),
  Len = 1,
  string_concat('0', Str, Output).

% format_time(+Time, -Output)
%% Gets the time value into a human readable string instead of the numeric value
format_time(Time, Output) :-
  T is round(Time),
  Hours is div(T, 60),
  Minutes is mod(T, 60),
  prepend_zero(Hours, OutputHour),
  prepend_zero(Minutes, OutputMins),
  string_concat(OutputHour, ':', TmpTime),
  string_concat(TmpTime, OutputMins, TmpOutput),
  prepend_zero(TmpOutput, Output).

% format_coordinates(+location(X, Y), -Output)
%% Gets the location input value in a string human readable format
format_coordinates(location(X, Y), Output) :-
  string_concat('(', X, Tmp1),
  string_concat(Tmp1, ',', Tmp2),
  string_concat(Tmp2, Y, Tmp3),
  string_concat(Tmp3, ')', Output).

% ordinal(+Num, -Output)
%% Prints special ordinal values for some numbers used in the pretty print
ordinal(Num, Output) :-
  Num = 1,
  Output = '1st'.

ordinal(Num, Output) :-
  Num = 2,
  Output = '2nd'.

ordinal(Num, Output) :-
  Num = 3,
  Output = '3rd'.

ordinal(Num, Output) :-
  Num = 101,
  Output = '101st'.

ordinal(Num, Output) :-
  Num = 102,
  Output = '102nd'.

ordinal(Num, Output) :-
  Num = 103,
  Output = '103rd'.

ordinal(Num, Output) :-
  Num = 201,
  Output = '201st'.

ordinal(Num, Output) :-
  Num = 202,
  Output = '202nd'.

ordinal(Num, Output) :-
  Num = 203,
  Output = '203rd'.

ordinal(Num, Output) :-
  Num \= 1,
  Num \= 2,
  Num \= 3,
  Num \= 101,
  Num \= 102,
  Num \= 103,
  Num \= 201,
  Num \= 202,
  Num \= 203,
  string_concat(Num, 'th', Output).

% drive_message(+Distance, +location(X, Y), -OutputMessage)
%% Obtains the message corresponding to the distance driven
%% to a set of coordinates for the location.
drive_message(Distance, location(X, Y), Output) :-
  string_concat('Drive ', Distance, Str1),
  string_concat(Str1, 'Km to the intersection of ', Str2),
  ordinal(X, XStr),
  string_concat(Str2, XStr, Str3),
  string_concat(Str3, ' avenue and ', Str4),
  ordinal(Y, YStr),
  string_concat(Str4, YStr, Str5),
  string_concat(Str5, ' street.', Output).

% get_next_orders(+Route, +Accumulator, -OrdersList)
%% Obtains the set of orders to be delivered after a depot
%% when the first element in the list is a depot.
get_next_orders([], Orders, Orders).

get_next_orders([Location|_], Acc, Orders) :-
  depot(Location, _, _),
  get_next_orders([], Acc, Orders).

get_next_orders([Location|Locations], Acc, Orders) :-
  order(Location, _, _, _),
  append(Acc, [Location], RAcc),
  get_next_orders(Locations, RAcc, Orders).

% print_orders_pickup(+OrdersList, +DepotID, +InitTime, -CurrTime, +InitLoad, -CurrLoad)
%% Prints the sequence of messages corresponding to the pickup process at a Depot.
%% Outputs the updated time and load weight after the load operation.
print_orders_pickup([], _ , CurrTime, CurrTime, CurrLoad, CurrLoad).

print_orders_pickup([OID|Orders], DepotID, InitTime, CurrTime, InitLoad, CurrLoad) :-
  depot(DepotID, _, Coords),
  format_time(InitTime, FInitTime),
  format_coordinates(Coords, FCoords),
  string_concat(InitLoad, 'Kg', StrLoad),
  string_concat('Pick up order ', OID, Str1),
  string_concat(Str1, ' from depot ', Str2),
  string_concat(Str2, DepotID, PickupMessage),
  tabbed_print([FInitTime, FCoords, StrLoad, PickupMessage]),
  load([OID], OrderWeight),
  NewTime is InitTime + 5,
  decimal_round(InitLoad + OrderWeight, 1, NewLoad),
  print_orders_pickup(Orders, DepotID, NewTime, CurrTime, NewLoad, CurrLoad).

%% Prints the corresponding messages for a route.
print_route_info([], _, _, _, _) :-
  nl.

% If at the final depot when empty route
print_route_info([DepotID|Route], _, InitialCoords, InitialTime, InitialLoad) :-
  depot(DepotID, _, Coords),
  Route = [],
  InitialCoords = Coords,
  format_time(InitialTime, FInitialTime),
  format_coordinates(Coords, FCoords),
  decimal_round(InitialLoad, 1, CurrentLoad),
  string_concat(CurrentLoad, 'Kg', StrLoad),
  string_concat('The vehicle didn\'t move the whole day.', '', Message),
  tabbed_print([FInitialTime, FCoords, StrLoad, Message]),
  print_route_info([], _, _, _, _).

% If at the final depot
print_route_info([DepotID|Route], VID, InitialCoords, InitialTime, InitialLoad) :-
  depot(DepotID, _, Coords),
  Route = [],
  InitialCoords \= Coords,
  decimal_round(InitialLoad, 1, CurrentLoad),
  string_concat(CurrentLoad, 'Kg', StrLoad),
  get_location(PrevLoc, InitialCoords),
  format_coordinates(InitialCoords, FInitialCoords),
  manhattan_distance(PrevLoc, DepotID, Distance),
  drive_message(Distance, Coords, DriveMessage),
  format_time(InitialTime, FInitialTime),
  tabbed_print([FInitialTime, FInitialCoords, StrLoad, DriveMessage]),
  driving_duration(VID, PrevLoc, DepotID, Duration),
  NewTime is InitialTime + Duration,
  format_time(NewTime, FNewTime),
  format_coordinates(Coords, FCoords),
  string_concat('Park at Depot ', DepotID, Message),
  tabbed_print([FNewTime, FCoords, StrLoad, Message]),
  print_route_info([], _, _, _, _).


% If at a depot print actions for depot.
print_route_info([DepotID|Route], VID, _, InitialTime, InitialLoad) :-
  depot(DepotID, _, Coords),
  get_next_orders(Route, [], Orders),
  Route \= [],
  print_orders_pickup(Orders, DepotID, InitialTime, CurrentTime, InitialLoad, CurrentLoad),
  print_route_info(Route, VID, Coords, CurrentTime, CurrentLoad).

% If delivering items print actions for orders.
print_route_info([OrderID|Route], VID, InitialCoords, InitialTime, InitialLoad) :-
  order(OrderID, _, Coordinates, _),
  format_time(InitialTime, FInitialTime),
  format_coordinates(InitialCoords, FInitialCoords),
  get_location(PrevLoc, InitialCoords),
  manhattan_distance(PrevLoc, OrderID, Distance),
  string_concat(InitialLoad, 'Kg', StrLoad),
  drive_message(Distance, Coordinates, DriveMessage),
  tabbed_print([FInitialTime, FInitialCoords, StrLoad, DriveMessage]),
  driving_duration(VID, PrevLoc, OrderID, Duration),
  NewTime is InitialTime + Duration,
  format_time(NewTime, FNewTime),
  format_coordinates(Coordinates, FCoordinates),
  string_concat('Deliver order ', OrderID, DeliverMessage),
  tabbed_print([FNewTime, FCoordinates, StrLoad, DeliverMessage]),
  load([OrderID], OrderWeight),
  decimal_round(InitialLoad - OrderWeight, 1, CurrentLoad),
  CurrentTime is NewTime + 5,
  print_route_info(Route, VID, Coordinates, CurrentTime, CurrentLoad).

% Auxiliary function to filter the list of origins
origin_for_vehicle(VID, vehicle_origin(VID, _)).

print_columns_header :-
  tabbed_print(['Time', 'Loc.', 'Load', 'Action']).

% print_vehicles_info(+VehiclesList, +DayID +OriginsList, +SchedulesList)
%% Print the information related to the list of vehicles for the input Schedules.
%% The list of schedules is usually expected to come filtered by a day.

print_vehicles_info([], _, _, _) :-
  nl, nl.

print_vehicles_info([VID|Vehicles], Day, Origins, Schedules) :-
  write('< Vehicle '),
  write(VID),
  write(' >'),nl, nl,
  print_columns_header,
  include(origin_for_vehicle(VID), Origins, [vehicle_origin(_, Origin)]),
  get_single_vehicle_route(VID, Origin, Schedules, Route),
  working_day(Day, StartTime, _),
  depot(Origin, _, Coords),
  print_route_info(Route, VID, Coords, StartTime, 0),
  print_vehicles_info(Vehicles, Day, Origins, Schedules).

% vehicle_origin_helper(+Index, +VehicleID, +WorkingDays, +Schedules, -Origin)
%% Helper function to retrieve the origin once the list of origins has been assembled
vehicle_origin_helper(Idx, VID, _, _, Origin) :-
  Idx = 0,
  vehicle(VID, Origin, _, _, _, _).

vehicle_origin_helper(Idx, VID, WorkingDays, Schedules, Origin) :-
  Idx \= 0,
  Index is Idx - 1,
  % Got to the day before and get last element from route:
  % Filter all schedules by previous day
  % Filter filtered schedules by vehicle
  % Get route for filtered schedules
  nth0(Index, WorkingDays, Day),
  include(schedule_in_working_day(Day), Schedules, WDSchedules),
  include(schedule_belongs_to_vehicle(VID), WDSchedules, VSchedules),
  get_single_vehicle_route(VID, VSchedules, Route),
  last(Route, Origin).

% vehicles_origins_for_day(+VehiclesList, +Day, +Schedules, +Accumulator, -Origins)
%% Retrieve the vehicle origins for a given day using the input Schedules as source.
%% Returns a list of the type:
%% [vehicle_origin(v1, origin1), vehicle_origin(v2, origin2) ...]
vehicles_origins_for_day([], _, _, Origins, Origins).

vehicles_origins_for_day([VID|Vehicles], Day, Schedules, Acc, Origins) :-
  findall(X, (working_day(X, _, _)), WorkingDays),
  sort(WorkingDays, SortedWorkingDays),
  nth0(Idx, SortedWorkingDays, Day),
  vehicle_origin_helper(Idx, VID, SortedWorkingDays, Schedules, Origin),
  append(Acc, [vehicle_origin(VID, Origin)], RAcc),
  vehicles_origins_for_day(Vehicles, Day, Schedules, RAcc, Origins) .

% print_days_info(+WorkingDaysList, +SchedulesList)
%% Iterates over the list of days to print the information related to the specific
%% day in the given schedules
print_days_info([], _) :-
  write('*** END OF PLANNING ***'), nl.

print_days_info([WDID|WorkingDays], Schedules) :-
  write('*** Schedule for Day '),
  write(WDID),
  write(' ***'),
  nl, nl,
  include(schedule_in_working_day(WDID), Schedules, WDSchedules),
  findall(X, (vehicle(X, _, _, _, _, _)), Vehicles),
  vehicles_origins_for_day(Vehicles, WDID, Schedules, [], Origins),
  print_vehicles_info(Vehicles, WDID, Origins, WDSchedules),
  print_days_info(WorkingDays, Schedules).

% pretty_print(+Plan)
%% Prints plan in a human readable format
pretty_print(plan(Schedules)) :-
  is_valid(plan(Schedules)),
  findall(X, (working_day(X, _, _)), WorkingDays),
  sort(WorkingDays, SortedWorkingDays),
  print_days_info(SortedWorkingDays, Schedules).
