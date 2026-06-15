---
layout: page
background: '/img/bg-wiki.jpg'
title: FAST-Python
authors:
- CyrilFerlicot
---


FAST-Python is a [FAST](FAST) meta-model used to represent AST of a Python modules.
It comes with a meta-model, an importer, a visitor and tools to manipulate and explore models.

## Importer

### Installation

FAST-Python comes with an importer based on TreeSitter's python parser.
To install it, execute the following script:

```smalltalk
Metacello new
	githubUser: 'moosetechnology' project: 'FAST-Python' commitish: 'main' path: 'src';
	baseline: 'FASTPython';
	load
```

You can add it to xour baseline like this:

```smalltalk
spec
	baseline: 'FASTPython'
	with: [ spec repository: 'github://moosetechnology/FAST-Python:main/src' ]
```

You can replace "main" by the tag you are interested in.

### Quick start

In order to parse a chain of character or a file you can do this:

```st
    FASTPythonImporter parse: 'if x > 0:
    if x < 10:
        1
    else:
        2
else:
    3'
```

Or

```st
    FASTPythonImporter parseFile: myFile
```

## Tools

### CFG

FAST-Python includes a CFG algorithm that can be used like this:

```smalltalk
FASTPythonCFGVisitor buildCFGOf: aModel allFunctionDefinitions first.

"or"

aModel allFunctionDefinitions first cfg
```


I can take one of five different entities to build a CFG:

-	a `FASTPyModule`
-	a `FASTPyFunctionDefinition`
-	a `FASTPyMethodDefinition`
-	a `FASTPyLambda`
-	a `FASTPyClassDefinition`



It is possible to build af "full" CFG via `#fullCfg`. A full CFG returns a dictionary with each definition encountered associated to their CFG. The first entry of the dictionary is the provided definition.
