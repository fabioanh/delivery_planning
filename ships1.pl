:- use_module(library(clpfd)).

ship(maria).
ship(victoria).
ship(elsa).

capacity(maria,5).
capacity(victoria,4).
capacity(elsa,3).

%container(Name,Size,AvailableTime,Destination)
container(a,2,3,boston).
container(b,4,7,baltimore).
container(c,1,10,boston).
container(d,1,16,miami).
container(e,4,17,baltimore).

travel_time(origin,miami,5).
travel_time(origin,boston,3).
travel_time(origin,baltimore,1).
travel_time(miami,boston,8).
travel_time(miami,baltimore,6).
travel_time(boston,miami,8).
travel_time(boston,baltimore,2).
travel_time(baltimore,miami,6).
travel_time(baltimore,boston,2).

% schedule(-Schedule)
%
% Schedule is list of schedule(Ship,Events).
% Events is a list where each element is one of the following:
%     pickup(ContainerName,Location,Time)
%     travel(Location1,Location2,ArrivalTime),
%     dropoff(ContainerName,Location,Time)
%% schedule(Schedule) :-
schedule(ShipCargos) :-
    findall(Ship,ship(Ship),Ships),
    findall(container(ContainerName,Size,AvailableTime,Destination),container(ContainerName,Size,AvailableTime,Destination),Containers),
    load_ships(Containers,Ships,[],ShipCargos).
	%% schedule(ShipCargos,[],Schedule).

% load_ships(+Containers,+Ships,+ShipCargosAcc,-ShipCargos)
%
% Calculates ship cargo configurations based on ship capacities and container sizes.
load_ships([],[],ShipCargos,ShipCargos).
load_ships(Containers,[Ship|Ships],ShipCargosAcc,ShipCargos) :-
    Containers \= [],
    capacity(Ship,Capacity),
    subset(Containers,Cargo),
    findall(Size,member(container(_,Size,_,_),Cargo),Sizes),
    sum(Sizes,#=<,Capacity),
    append(ShipCargosAcc,[cargo(Ship,Cargo)],ShipCargosAcc2),
    subtract_once(Containers,Cargo,RemainingContainers),
    load_ships(RemainingContainers,Ships,ShipCargosAcc2,ShipCargos).

% subset(+List,-Subset)
%
% Returns all ordered subsets of the given list.
subset([], []).
subset([E|Tail], [E|NTail]):-
  subset(Tail, NTail).
subset([_|Tail], NTail):-
  subset(Tail, NTail).

% subtract_once(+List,+DeleteList,-NewList)
%
% Removes each element from DeleteList from List. Unlike the predicate from the lists
% library, this only removes the first instance of each element in DeleteList from List.  
subtract_once(List, [], List).
subtract_once(List, [Item|Delete], Result):-
  (select(Item, List, NList)->
    subtract_once(NList, Delete, Result);
    (List\=[],subtract_once(List, Delete, Result))).