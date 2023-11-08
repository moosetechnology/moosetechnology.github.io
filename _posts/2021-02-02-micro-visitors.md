---
layout: post
title: "Micro-Visitors for Parsing Programming Languages"
date: 2021-01-26 23:45:13 -0400
background: '/img/posts/bg-posts.jpg'
author: Nicolas Anquetil
comments: true
---

For Moose, I had to design a number of parsers for various languages ([Java](https://github.com/NicolasAnquetil/VerveineJ),
[Ada](https://github.com/NicolasAnquetil/Ada2Famix),
C/C++,
[PowerBuilder](https://github.com/moosetechnology/PowerBuilderParser)).
If you have already done that, you will know that the Visitor pattern is a faithful ally.
To help me in this, I came with the concept of "micro visitor" allowing to modularize visitors.

## Parsing and Visitors

[Parsing source code](https://en.wikipedia.org/wiki/Parsing) starts with a  grammar of the programming language and an actual parser that creates an Abstract syntax Tree (AST) of the program.

For many programming languages, the AST can contain tens of different nodes.
The way to master this complexity is to use visitors.
A visitor is a class with one method (traditionaly `visitXYZ(XYZ node)`) for each possible type of node in the AST.
Each method treats the current node and delegates to other methods treating the nodes below it.

For a parser like [VerveineJ](https://github.com/NicolasAnquetil/VerveineJ) (Java to MSE importer) the visitor class reached [2000 lines of code](https://github.com/NicolasAnquetil/VerveineJ/blob/4adb83c61af9791fb140c7e636ca3aabca41ba71/src/eu/synectique/verveine/extractor/java/VerveineVisitor.java) and became difficult to maintain as there are also interactions between visiting methods because the treatment of a node down in the AST may depend on what are its parent nodes.
For example, in Java, `ThisExpression` node may be found in different situations:
- Return the instance running the current method: `this.attribute`
- Return the enclosing object of the current instance: `AClass.this`
- Invoke current class constructor: `this(...)`

Therefore the treatment in `visitThisExpression( ThisExpression node)` may depend on which method called it.
This makes it more complex to develop and maintain all the "visitXYZ" methods.

## Micro-Visitor

On the other hand, a visitor typically has a small state:

- the name of the file being parsed;
- a context stack of the visit (_eg_ visiting a method, inside a class, inside a file);
- a model of the result being built by the visitor (*eg* a Moose model).

As a result, I came up with the notion of **micro-visitors** specialized for a single task.
For example, for VerveineJ, I have 10 (concrete) micro-visitors, 4 to create entities and 6 to create dependencies between them:

- `VisitorPackageDef`, creating Famix packages;
- `VisitorClassMethodDef`, creating Famix classes and methods;
- `VisitorVarsDef`, creating Famix attribute, parameter, local variable definition;
- `VisitorComments`, creating comments in all Famix entities;
- `VisitorInheritanceRef`, creating inheritances between classes
- `VisitorTypeRefRef`, creating reference to declared types;
- `VisitorAccessRef`, creating accesses to variables;
- `VisitorInvocRef`, creating invocation dependencies between methods;
- `VisitorAnnotationRef`, handling annotations on entities;
- `VisitorExceptionRef`, handling declared/catched/thrown exceptions.

The resulting visitors are much smaller (around 600 lines of code for the three more complex: `VisitorInvocRef`, `VisitorClassMethodDef`, `VisitorAccessRef` ; less than 150 lines of code for `VisitorPackageDef` and `VisitorExceptionRef`) and thus easier to define and maintain.
Also, because the visitor is specialized, there are less dependencies between the methods: `VisitorInvocRef` only treats `ThisExpression` when it is a constructor invocation.

## Using Micro-Visitors

The overhead on the execution is small as each visitor is specialized and does not need to go through all the AST (_eg_ a visitor for function declaration in C would not have to visit the body of these functions since they cannot contain other function declarations).

Micro-visitors can be used independantly one of the other (in _sequence_) as in VerveineJ where each visitor is called one after the other (by the [FamixRequestor](https://github.com/NicolasAnquetil/VerveineJ/blob/master/src/fr/inria/verveine/extractor/java/FamixRequestor.java) class) to visit the full AST.
The "orchestrator" object owns the state and pass it to each visitor in turn.

Micro-visitors can also call one another (in _delegation_).
For example for [PowerBuilder](https://github.com/moosetechnology/PowerBuilderParser), there is one main visitor (`PowerBuilder-Parser-Visitor.PWBCompilationUnitVisitor`, visiting the AST for a source file) and 7 (concrete) micro-visitors:
- `PWBTypeDeclarationVisitor`, visiting type declarations;
- `PWBBehaviouralDeclarationVisitor`, visiting function/routine definitions;
- `PWBVariableDeclarationVisitor`, visiting declarations of all kind of variables;
- `PWBTypeReferenceToIdentifierVisitor`, visiting references to type names (for example in variable declarations);
- `PWBStatementsVisitor`, visiting statements in the body of behaviourals;
- `PWBExpressionsVisitor`, visiting expressions in statements;
- `PWBBehaviouralInvocationVisitor`, visiting the invocation of behavioural in expressions.

In this case, the main visitor (`PWBCompilationUnitVisitor`) owns the state and its auxiliary visitors get this state from their respective parent visitor:
- `PWBCompilationUnitVisitor` spawns a `PWBBehaviouralDeclarationVisitor` when it encounters a function definition, this one spawns a `PWBStatementsVisitor` to visit the body of the function, `PWBStatementsVisitor` spawns a `PWBExpressionsVisitor` to visit expressions found in the statements.
- if the `PWBExpressionsVisitor` needs to access the context stack, it asks to its parent `PWBStatementsVisitor`, that asks to its parent `PWBBehaviouralDeclarationVisitor`, that asks to the `PWBCompilationUnitVisitor`.
