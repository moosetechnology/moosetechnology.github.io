---
layout: page
background: '/img/bg-wiki.jpg'
title: FAST-Pharo
---

# FAST-Pharo <!-- omit in toc -->

FAST-Pharo is a [FAST](FAST) meta-model used to represent AST of a Pharo class, trait or method.
It comes with a meta-model, an importer, and tools to manipulate and explore models.

- [FAST-Pharo meta-model](#fast-pharo-meta-model)
- [Importer](#importer)
  - [Installation](#installation)
  - [Import](#import)
- [Tools](#tools)

## FAST-Pharo meta-model

![FAST-Pharo meta-model](https://raw.githubusercontent.com/moosetechnology/FAST-Pharo/v2-doc/fast-pharo.svg)

[Full Image](https://raw.githubusercontent.com/moosetechnology/FAST-Pharo/v2-doc/fast-pharo.svg)

## Importer

### Installation

FAST-Pharo comes with an importer defined with the SmalltalkImporter based on [PetitParser](https://github.com/moosetechnology/PetitParser) moose project parser.
To install it, execute the following script:

> It can take time

```smalltalk
Metacello new
    baseline: 'FASTPharo';
    repository: 'github://moosetechnology/FAST-Pharo:v2/src';
    load: 'importer'.
```

This script installs the [FAST](FAST) project, the PetitParser project, the FAST-Pharo project, and all the dependencies.

### Import

The FAST-Pharo importer allows one to import AST of a method or of a class.
To import method, one **must** use the method `runWithSource:`.

For example, the following script import the method `testVisitInheritance` of `FamixMMUMLDocumentorVisitorTest`

```smalltalk
FASTSmalltalkImporterVisitor

	new runWithSource:
		(FamixMMUMLDocumentorVisitorTest >> #testVisitInheritance)
			sourceCode
```

## Tools

One nice way to explore a FAST model is to use the source code and the tree extensions of the inspector.
It allows one the navigate in a FAST model and see the code corresponding to each node.

To use it, execute the code to create a model with <kbd>Ctrl</kbd>+<kbd>I</kbd>.
It opens an inspector on the imported model.

Then, select a method entity.
On the right-hand pane of the inspector, select the *Tree* tab, on the left-hand pane, select the source code extension.
The source code is highlighted and the area selected corresponds to the entity selected in the right-hand panel.

![Example navigation in FAST](img/navigate-fast-pharo.gif)
