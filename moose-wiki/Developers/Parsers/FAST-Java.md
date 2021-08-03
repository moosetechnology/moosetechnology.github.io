---
layout: page
background: '/img/bg-wiki.jpg'
title: FAST-Java
---

# FAST-Java <!-- omit in toc -->

FAST-Java is a [FAST](FAST) meta-model used to represent AST of a Java class or method.
It comes with a meta-model, an importer, and tools to manipulate and explore models.

- [FAST-Java meta-model](#fast-java-meta-model)
- [Importer](#importer)
  - [Installation](#installation)
  - [Import](#import)
- [Tools](#tools)

## FAST-Java meta-model

![FAST-Java meta-model](https://raw.githubusercontent.com/moosetechnology/FAST-JAVA/8ceb4e8d23bda1c57a151879b7fae50cd6fdb290/fast-java.svg){: .img-fill }

[Full Image](https://raw.githubusercontent.com/moosetechnology/FAST-JAVA/8ceb4e8d23bda1c57a151879b7fae50cd6fdb290/fast-java.svg)

## Importer

### Installation

FAST-Java comes with an importer defined with the [SmaCC](https://github.com/j-brant/SmaCC) parser.
To install it, execute the following script:

> It can take time

```st
Metacello new
    baseline: 'FASTJava';
    repository: 'github://moosetechnology/FAST-Java:v2/src';
    load: 'smacc'.
```

This script installs the [FAST](FAST) project, the SmaCC project, the FAST-Java project, and all the dependencies.

### Import

The FAST-Java importer allows one to import AST of a method or of a class.
To import method, one **must** use the method `parseCodeMethodString:`, whereas, to import class, one **must** use the method `parseCodeString:`.

For example, the following script import the method `onClick`

```st
JavaSmaCCProgramNodeImporterVisitor new parseCodeMethodString: 'public void onClick(ClickEvent event) {
  view.setDisplayView(view.new InnerClass());
}'
```

## Tools

One nice way to explore a FAST model is to use the source code and the tree extensions of the inspector.
It allows one the navigate in a FAST model and see the code corresponding to each node.

To use it, execute the code to create a model with <kbd>Ctrl</kbd>+<kbd>I</kbd>.
It opens an inspector on the imported model.

Then, select a method entity.
On the right-hand pane of the inspector, select the *Tree* tab, on the left-hand pane, select the source code extension.
The source code is highlighted and the area selected corresponds to the entity selected in the right-hand panel.

![Example navigation in FAST](img/navigate-fast.gif){: .img-fill }
