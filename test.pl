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




% order_value(+ProductsDetailsList, +Value, -Result)
%% Recursive function used to go through a list of order details, get the products information and compute the total order value.
order_value([], Value, Value).

order_value([ProductDetails|ProductsDetails], Value, Result) :-
  ProductID/Quantity = ProductDetails,
  product(ProductID, Val, _),
  R is Value + (Val * Quantity),
  order_value(ProductsDetails, R,  Result).

% discount_factor(+Day, +Deadline, -Factor)
%% Check if the given Day is before the Deadline and assign the proper discount factor when this condition is not met.
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
%% Give a list of order IDs [o1, o2, ..., oK] and Acc = 0 in order to obtain the total weight of the listed orders.
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
  !.

subtract_value(Product/Quantity, [_|Products], Result) :-
  subtract_value(Product/Quantity, Products, Result),
  !.

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
  NewInventory = ResultInventory,
  !. % Validate if the first value is enough, if multiple values are required see how to deal with the false returned.
