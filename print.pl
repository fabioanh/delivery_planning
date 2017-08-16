:- module(print, [pretty_print/1]).

:- use_module(core).
:- use_module(valid).

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
  writeln('*** END OF PLANNING ***').

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
