# Delivery Planning
Prolog implementation of a delivery planning problem. Solution for the final project of Declarative Programming 2016-2017.

The esence of the problem is to try to organize the schedule for a set of vehicles moving in a grid with fixed locations for depots, picking up orders to be delivered to locations inside the grid. All these operations being done within a working time during a set of Working Days. This being said, there is a set of constraints on the way the routes have to be followed and on the vehicles (load and speed among others). The full description of the problem can be found [here](https://ai.vub.ac.be/node/1546)

## Assignment goals
The assignment is divided in a set of basic, core and extra goals. The current implementation gives solution to the set of basic and half the core ones. Unfortunatelly the most important goal of the assignment was not achieved, the software doesn't purpose valid solutions for the problem given the input databases.

## How to run it?
Make sure Swi-pl is installed on your environment. If you need further information on how to do it [the official website](http://www.swi-prolog.org/) contains useful instructions and data that can be handy for your needs.
In order to run the developed predicates, go to the root of the project, pick the instance file you want to run the predicates on and set it as the first parameter for the following command that will run in the command line.
```
swipl ../instances/<file> load.pl
```
The **load.pl** file contains a reference to the modules used in the solution. Additional extra files are not referenced here due to the fact that they were different attempts to solve the problem or simple test code that at the end didn't find a place in the solution.

## Solution Description
The implementation presents a solution to different questions performed on top of the database through the following predicates:

* **driving_duration(+VID,+FromID,+ToID,-Duration)**:

    Computes the duration used by the vehicle identified by _VID_ to go from the point _FromID_ to _ToID_. These two locations can be either a *Depot* or an *Order delivery location*. An example running the predicate to obtain a distance would look like this:
    ```prolog
    driving_duration(v1,d1,d2,X).
    X = 240.
    ```

* **earning(+OID,+Day,-Value)**:

    Gives the Value obtained for delivering an Order identified by its ID _OID_. The Original earnings of delivering an order are reduced by half when the order is not delivered on time. An example running this predicate will look like this:
    ```prolog
    earning(o1,3,X).
    X = 192.5.
    ```
      
* **load(+OrdersList,-Weight)**:

    Given a list of order identifiers _OrdersList_ returns the total weight of the orders in the output variable _Weight_. The weight value is computed based on the list of products contained by the order. Each of the products has a weight value and a quantity that are used to compute the final required value. An example of this predicate looks like this:
    ```prolog
    load([o1,o2],X).
    X = 200.7.
    ```

* **update_inventory(+Inventory,?OID,?NewInventory)**:
    
    _Inventory_ is a list of terms of the form **PID/Q** where PID is a product identifier and Q is an integer value that indicates the quantity. A list of these terms will look like this: `[p1/4,p2/2,p5/8]`. This predicate subtracts the products information of an order identified by _OID_ from a list of _Products/Quantities_ giving the result in the third component of the functor _NewInventory_. A usage example will look like this:
    ```prolog
    update_inventory([p1/50,p3/10,p2/10],o1,X).
    X = [p1/50, p3/3, p2/10] ;
    ```
    When the _OID_ value in the functor call is not bounded this value returns the possible order IDs matching the inventory requirements. This can be appreciated in the followin example:
    ```prolog
    update_inventory([p1/50,p3/10,p2/10],X,[p1/40,p3/10,p2/2]).
    X = o3 ;
    ```
    When the _OID_ value is not bounded there is a problem with this implementation. The order of the inventory given for the third member affects the result. If in the last example the order of the list varies like this `[p1/40,p2/2,p3/10]` the order 3 will not be identified.

## Intended Solution Proposal
