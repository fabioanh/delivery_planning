:- module(profit, [profit/2]).

:- use_module(core).

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

