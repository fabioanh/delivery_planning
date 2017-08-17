:- module(solution, [ get_order_depots/1,
                      valid_products_quantity/2,
                      is_valid/1]).

:- use_module(core).
:- use_module(profit).
:- use_module(library(clpfd)).



aux_unique_schedule_per_working_day([], _, _).

aux_unique_schedule_per_working_day([vehicle(VID, _, _, _, _, _)|Vehicles], DayID, Schedules) :-
  %% filter(Schedules, DayID, VID, WDSchedules),
  %% filter_schedules_by_working_day_and_vehicle(Schedules, DayID, VID, [], WDSchedules),
  select(schedule(VID, DayID, _), Schedules, R),    % I can remove X from L giving R, and
  \+ member(schedule(VID, DayID, _), R),
  %% WDSchedules = [schedule(VID, DayID, _)],
  aux_unique_schedule_per_working_day(Vehicles, DayID, Schedules).

unique_schedule_per_working_day([], _, _).

unique_schedule_per_working_day([working_day(DayID, _, _)|WorkingDays], Vehicles, Schedules) :-
  aux_unique_schedule_per_working_day(Vehicles, DayID, Schedules),
  %% include(schedule_in_working_day(DayID), Schedules, WDSchedules),
  unique_schedule_per_working_day(WorkingDays, Vehicles, Schedules).

%overhead(+OrderID, -Overhead)
%% Gets the overhead for picking up and delivering an order => 5 mins for each operation
overhead(ID, Overhead) :-
  \+ order(ID, _, _, _),
  Overhead is 0.

overhead(ID, Overhead) :-
  order(ID, _, _, _),
  Overhead is 10.

%% loop through the Route, add the driving_duration between every two consecutive points
%% and subtract it from the TimeLeft
is_route_on_time([], _, _).

is_route_on_time([_], _, _).

is_route_on_time([Current, Next | Rest], Pace, TimeLeft) :-
  Current \= Next,
  manhattan_distance(Current, Next, Distance),
  overhead(Current, Overhead),
  TLeft is TimeLeft - (Pace * Distance) - Overhead,
  TLeft >= 0,
  is_route_on_time([Next|Rest], Pace, TLeft).

% aux_schedules_on_time(+WorkingDaysList, +Vehicle, +Origin, +Schedules)
%% Auxiliary functor to iterate over the working days checking that each of the schedules is on time for the current
%% working day based on the pace of the vehicle and the route information contained in the schedule.
aux_schedules_on_time([], _, _, _).

aux_schedules_on_time([working_day(DayID, Start, End)|WorkingDays], vehicle(VID, _, _, Pace, _, _), Origin, Schedules) :-
  include(schedule_belongs_to_vehicle(VID), Schedules, VSchedules),
  include(schedule_in_working_day(DayID), VSchedules, [schedule(VID, DayID, Route)]),
  TimeWindow is End - Start,
  is_route_on_time([Origin|Route], Pace, TimeWindow),
  last([Origin|Route], NewOrigin),
  aux_schedules_on_time(WorkingDays, vehicle(VID, _, _, Pace, _, _), NewOrigin, Schedules).

% schedules_on_time(+VehiclesList, +WorkingDaysList, +Schedules)
%% Iterates over the vehicles to check that their schedules are all on time over the whole list of working days
schedules_on_time([], _, _).

schedules_on_time([Vehicle|Vehicles], WorkingDays, Schedules) :-
  vehicle(_, Origin, _, _, _, _) = Vehicle,
  aux_schedules_on_time(WorkingDays, Vehicle, Origin, Schedules),
  schedules_on_time(Vehicles, WorkingDays, Schedules).

%% Schedules goruped by working day are valid if:
%%    - Each day's schedule's routes are on time
%%    - Each order is delivered only once or not at all
%%    - There is only one schedule per vehicle per working day
are_schedules_valid_by_working_day(Schedules) :-
  get_working_days(WorkingDays),
  get_vehicles(Vehicles),
  schedules_on_time(Vehicles, WorkingDays, Schedules),
  unique_schedule_per_working_day(WorkingDays, Vehicles, Schedules).

%% A plan is valid if:
%%    - Each schedule has a valid structure and data by itself
%%    - Schedules grouped by working day are valid
is_valid(plan(Schedules)) :- 
  %% are_schedules_valid(Schedules), % Under development -> code commented out below
  are_schedules_valid_by_working_day(Schedules).
















%% solution(Schedule) :-
%%  schedule(Schedule).

%% A Route is valid if:
%%    - The Route is empty
%%    OR
%%    - The Route doesn't exceed the weight requirements for the vehicle
%%    - The products requested align with the depots' inventories
%%    - The last stop in the route is a Depot
%%    - The Route has no consecutive identical locatoins
%%    - 
%% is_route_valid(_, _, []).

%% is_route_valid(VID, DayID, Route) :-
%%   vehicle(VID, _, _, _, _, _),
%%   working_day(DayID, _, _)
%%   Route is.

%% %% A single schedule is valid if by itself if:
%% %%    - It is related to an existent Vehicle 
%% %%    - It happens in a Working Day 
%% %%    - The Route is valid
%% %%    - 
%% are_schedules_valid([schedule(VID, DayID, Route)|Schedules]) :-
%%   vehicle(VID, Origin, _, _, _, _),
%%   working_day(DayID, _ ,_).
  %% is_route_valid(Route).

% filter_schedules_by_working_day_and_vehicle(+SchedulesList, +DayID, +VID +Accumulator, -ResultList) :-
%% Gets the sublist of schedules happening on the given DayID and for the vehicle VID
%% filter_schedules_by_working_day_and_vehicle([], _, _, WDSchedules, WDSchedules).

%% filter_schedules_by_working_day_and_vehicle([Schedule|Schedules], DayID, VID, Acc, WDSchedules) :-
%%  Schedule \= schedule(VID, DayID, _),
%%  filter_schedules_by_working_day_and_vehicle(Schedules, DayID, VID, Acc, WDSchedules).

%% filter_schedules_by_working_day_and_vehicle([Schedule|Schedules], DayID, VID,  Acc, WDSchedules) :-
%%  Schedule = schedule(VID, DayID, _),
%%  append(Acc, [Schedule], RAcc),
%%  filter_schedules_by_working_day_and_vehicle(Schedules, DayID, VID, RAcc, WDSchedules).

%% filter([], _, []).
%% filter([S|List], DayID, VID, [S|Filtered]) :-
%%     /* Conditions are met: bypass the element to the filtered list */
%%     S = schedule(VID, DayID, _),
%%     filter(List, DayID, Filtered).
%% filter([S|List], DayID, VID,Filtered) :-
%%     /* Conditions are not met: do not include the element in the filtered list */
%%     S \= schedule(VID, DayID, _),
%%     filter(List, DayID, VID, Filtered).