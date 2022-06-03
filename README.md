# SIMPL-To-A-PRIMPL-Compiler
Run Q9 using DrRacket ide to use compiler. 
SIMPL is an imperative programming laguage made in class which.
Each program is of the form  (vars [(id number) (id number)] stmt ...).
Where the list of (id number) are parameters being passed into the program where id is the name and number the value fo the variable.
stmt... is then a sequence of SIMPL commands that will be executed.
A-PRIMPL is an assemply like language which does not have nested expressions, loops or if statements.
Instead nested arithmetic and boolean expressions can be implemented using temporary variables on a stack.
In order to make loops and if statements the goto commands and labels are used.
Declaring variables is also done without beinga ble to point to any address in memory. Rather the variables are declared after the executable code is done
as static memory.
