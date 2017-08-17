# Delivery Planning
Prolog implementation of a delivery planning problem. Solution for the final project of Declarative Programming 2016-2017.

The esence of the problem is to try to organize the schedule for a set of vehicles moving in a grid with fixed locations for depots, picking up orders to be delivered to locations inside the grid. All these operations being done within a working time during a set of Working Days. This being said, there is a set of constraints on the way the routes have to be followed and on the vehicles (load and speed among others). The full description of the problem can be found [here](https://ai.vub.ac.be/node/1546)

## Assignment goals
The assignment is divided in a set of basic, core and extra goals. The current implementation gives solution to the set of basic and half the core ones. Unfortunatelly the most important goal of the assignment was not achieved, the software doesn't purpose valid solutions for the problem given the input databases.

## How to run it?
Make sure Swi-pl is installed on your environment. If you need further information on how to do it [the official website](http://www.swi-prolog.org/) contains useful instructions and data that can be handy for your needs.
In order to run the developed predicats, go to the root of the project, pick the instance file you want to run the predicates on and set it as the first parameter for the following command that will run in the command line.
```
swipl ../instances/<file> load.pl
```
The **load.pl** file contains a reference to the modules used in the solution. Additional extra files are not referenced here due to the fact that they were different attempts to solve the problem or simple test code that at the end didn't find a place in the solution.

