---
title: "Control Flow Graph for FAST Fortran"
date: 2025-03-08
background: './img/posts/bg-posts.jpg'
authors:
- NicolasAnquetil
comments: true
tags:
- Famix-tools
- FAST
---

## A Control Flow Graph analysis for FAST Fortran

Control Flow Graphs (CFG) are a common tool for static analyzis of a computation unit (eg. a method) and find some errors (unreachable code, infinite loops)

It is based on the concept of *Basic Block*: a sequence of consecutive statements in which flow of control can only enter at the beginning and leave at the end. Only the last statement of a basic block can be a branch statement and only the first statement of a basic block can be a target of a branch.

There are two distinctive basic blocks: 
- Start Block: The entry block allows the control to enter into the control flow graph. There should be only one start block.
- Final Block: Control flow leaves through the exit block. There may be several final blocks.

The package `FAST-Fortran-Analyses` in [https://github.com/moosetechnology/FAST-Fortran](https://github.com/moosetechnology/FAST-Fortran) contains classes to build a CFG of a Fortran program unit (a main program, a function, or a subroutine).

### Creating the FAST Model

We must first create a FAST model of a Fortran program.
For this we need an external parser.
We currently use `fortran-src-extras` from [https://github.com/camfort/fortran-src-extras](https://github.com/camfort/fortran-src-extras).

To run it on a fortran file you do:
```smalltalk
fortran-src-extras serialize -t json -v77l encode <fortran-file.f>
```
This will produce a json AST of the program that we can turn into a FAST-Fortran AST.

If you have fortran-src-extras installed on your computer, all this is automated in FAST-Fortran
```smalltalk
 <fortran-file.f> asFileReference
	readStreamDo: [ :st |
		FortranProjectImporter new getFASTFor: st contents ]
```
This script will create an array of ASTs from the <fortran-file.f> given fortran file.
If there are several program units in the file, there will be several FAST models in this array.
In the example below, there is only one program, so the list contains only the AST for this program.

We will use the following Fortran-77 code:
```fortran
      PROGRAM EUCLID
*     Find greatest common divisor using the Euclidean algorithm
        PRINT *, 'A?'
        READ *, NA
        IF (NA.LE.0) THEN
          PRINT *, 'A must be a positive integer.'
          STOP
        END IF
        PRINT *, 'B?'
        READ *, NB
        IF (NB.LE.0) THEN
          PRINT *, 'B must be a positive integer.'
          STOP
        END IF
        IA = NA
        IB = NB
    1   IF (IB.NE.0) THEN
          ITEMP = IA
          IA = IB
          IB = MOD(ITEMP, IB)
          GOTO 1
        END IF
        PRINT *, 'The GCD of', NA, ' and', NB, ' is', IA, '.'
        STOP
        END
```

### Creating the CFG

From the FAST model above, we will now create a Control-Flow-Graph:
```smalltalk
 <FAST-model> accept: FASTFortranCFGVisitor new
 ```

The class `FASTFortranCFGVisitor` implements an algorithm to compute basic blocks from [https://en.wikipedia.org/wiki/Basic_block](https://en.wikipedia.org/wiki/Basic_block).

This visitor goes throught the FAST model and creates a list of basic blocks that can be inspected with the `#basicBlocks` method.

There is a small hierarchy of basic block classes: 
- `FASTFortranAbstractBasicBlock`, the root of the hierarchy.
  It contains `#statements` (which are FAST statement nodes).
  It has methods to test its nature: `isStart`, `isFinal`, `isConditional`.
    It defines an abstract method `#nextBlocks` that returns a list of basic blocks that this one can directly reach.
    Typically there are  1 or 2 next blocks, but Fortran can have more due to "arithmetic IF", "computed GOTO" and "assigned GOTO" statements.
- `FASTFortranBasicBlock`, a common basic block with no branch statement.
  If it is final, its  `#nextBlocks` is empty, otherwise it's a list of 1 block.
- `FASTFortranConditionalBasicBlock`, a conditional basic block.
  It may reach several `#nextBlocks`, each one associated with a value, for example `true` and `false`.
  The method `#nextBlockForValue:` returns the next block associated to a given value.
  In our version of CFG, a conditional block may only have one statement (a conditional statement).
  
You may have noticed that our blocks are a bit different from the definition given at the beginning of the blog-post:
- our "common" blocs cannot have several next, they never end with a conditional statement;
- our conditional blocks can have only one statement.

For the program above, the CFG has 10 blocks.
- the first block is a common block and contains 2 statements, the PRINT and the READ;
- its next bloc is a conditional block for the IF.
  It has 2 next blocs:
  - `true` leads to a common block with 2 statements, the PRINT and the STOP. This is a final block (STOP ends the program);
  - `false` leads to the common block after the IF
- ...

### Visualize the CFG

As a first analysis tool, we can visualize the CFG.
Inspecting the result of the next script will open a Roassal visualization on the CFG contained in the `FASTFortranCFGVisitor`.

```smalltalk
FASTFortranCFGVisualization on: <aFASTFortranCFGVisitor>
```

For the program above, this gives the visualization below.
- the dark dot is the starting block (note that it is a block and contains statements);
- the hollow dots are final blocks;
- it's not the case here, but a block may also be start and final (if there are no conditional blocks in the program) and this would be represented by a "target", a circle with a dot inside;
- a grey square is a comon block;
- a blue square is a conditional block;
- hovering the mouse on a block will bring a pop up with the list of its statements (this relies on the `FASTFortranExporterVisitor`)

!["Viualizing the Control Flow Graph"](./img/posts/2025-03-08-cfg/the-CFG.png)

One can see that:
- the start block has 2 associated statements (PRINT and READ);
- there are several final blocks, due to the STOP statements;
- there is a loop at the bottom left of the graph where the last blue conditional block is "IF (IB.NE.0)" and the last statement of the grey block (`true` value of the IF), is a GOTO.

### Other analyses

There are little analyses for now on the CFG, but `FASTFortranCFGChecker` will compute a list of unreachableBlocks that would represent dead code.

Control flow graphs may also be used to do more advanced analyses and possibly refactor code.
For example, we mentioned the loop at the end of our program implemented with a IF statement and a GOTO.
This could be refactored into a real WHILE loop that would be easier to read.

This is left as an exercise for the interested people :wink:

### Adapting to other languages

Building a control flow graph is language dependant to identify the conditional statements, where they lead, and the final statements.

But much could be done in FAST core based on `FASTTReturnStatement` and a (not yet existing at the time of writing) `FASTTConditionalStatement`.

Inspiration could be taken from `FASTFortranCFGVisitor` and the process is not overly complicated.
It would probably be even easier for modern languages that do not have the various GOTO statements of Fortran.

Once the CFG is computed, the other tools (eg. the visualization) should be completely independant of the language.

All hands on deck!
