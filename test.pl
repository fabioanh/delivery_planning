% manhattan_distance(+location(X1, Y1), +location(X2, Y2), -Distance)
%% Gives the manhattan distance in kilometers for the X and Y given coordinates
manhattan_distance(location(X1, Y1), location(X2, Y2), Distance) :-
  XDis is abs(X1 - X2),
  YDis is abs(Y1 - Y2),
  Distance is float(XDis + YDis).

% get_location(+ResourceID, -location(X, Y))
%% Finds the location of either depots or packages given the identifier of the resource.
get_location(ResourceID, location(X, Y)) :-
  depot(ResourceID, _, location(X,Y)).

get_location(ResourceID, location(X, Y)) :-
  order(ResourceID, _, location(X,Y), _).

% decimal_round(+Val, +DecimalPositions, -Rounded)
%% Rounds a float value to the desired number of decimal positions
decimal_round(Val, DecimalPositions, Rounded) :-
  RoundFactor is (10 ** DecimalPositions),
  Tmp is round(Val * RoundFactor),
  Rounded is Tmp / RoundFactor.

% driving_duration(+VID,+FromID,+ToID,-Duration).
%% Gives the duration in minutes of a vehicle VID, spent going from the depot/oder FromID to the depot/order ToID
driving_duration(VID, FromID, ToID, Duration) :-
  get_location(FromID, Origin),
  get_location(ToID, Destination),
  manhattan_distance(Origin, Destination, Distance),
  vehicle(VID, _, _, Pace, _, _),
  decimal_round(Distance * Pace, 1, Duration).
