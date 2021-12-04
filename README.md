# Minesweeper-o-Matic

This project is the final assignment of the module CSU44012-202122, Topics in functional programming, in Trinity College Dublin, under the supervision of Mr. Glenn Strong. See below for building and launching the app.

## Assignment

The final project for CS4012 requires you to build an auto-playing implementation of the game minesweeper; use Threepenny to build the user interface and provide as complete an automatic player as you can.

### Workphases 
- Phase 1
    - Model the game of Minesweeper in Haskell. With no UI or interaction model this part of the project is simple enough.
    - Build a UI for the program from phase 1 using Threepenny GUI. You can specify the details of how the user interacts to play the game (i.e. what the game looks like and exactly what controls are used to select locations and clear or flag them). It's probably best to keep this simple.

For Phase 1 is it 
- *necessary* that :
    - The program must distribute mines randomly around the grid
    - Allow the user to uncover and flag mines (interactively)
    - Detect the endgame condition

- Phase 2
    - Add a 'play move' button to the game from phase 1. Pressing this button will attempt to play a good, safe, move. I will note that Minesweeper is known to be NP-complete, which means that a well written and efficient solver is, well, hard! That link contains some very good commentaries and links on solving minesweeper. Check out his PDF presentation linked there.

For Phase 2 it is 
- *necessary* that :
    - The program play a move automatically if there is an unambiguously safe move available
- *desirable* that :
    - If a safe move is not available the program will play the least dangerous move available (see the links below for some discussion of the probabilities).

### Documentation
You need to document your program clearly:
- Include suitable comments wherever there is a part of the program that deserves greater explanation
- Include a short document (text or PDF), about 2-3 pages will suffice, outlining the high level design choices you made, indicating how many of the project deliverables were satisfactorily completed. This should also include also a short (one or two paragraphs paragraph) reflection on the process of designing the program. Some questions you might consider here include: how suitable was Haskell as a language for tackling each phase of the project, what was your experience of the software development process (including things like testing and debugging), and what features of the language proved useful. 

### Grading
The project is worth 35% overall :
- 15% of the marks will be given for the documentation
- The basic implementation (phase 1) is worth up to 50% of the final marks. Your program needs to be :
    - Well designed and commented
    - Accompanied by suitable documentation (this might be in the form of literate source files, or it may feature additional documents).
- The auto-player is worth 35% of the marks. At a minimum your player should be able to make simple and obvious moves (i.e. spot a few of the basic patterns as described in the Minesweeper wiki). The more sophisticated the player the better! Remember that you don't need to solve the whole game here (that's extremely hard), but try to implement at least one advanced tactic.

## Building the application

To build this application, make sure ```stack``` is correctly installed on your computer. Git clone or download the sources of the project using this repository, then ```cd``` to the root of this project (where this Readme.md is), then use the following :

```{bash}
stack build && stack exec shape-server-exe
```

Then, you can either follow the link given in the Terminal from which you used ```stack```, or you can point a browser to ```http://127.0.0.1:8023/```, being the default configuration.
