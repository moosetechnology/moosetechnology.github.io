---
title: "Generating a visitor infrastructure for a given meta-model"
date: 2025-02-26
background: './img/posts/2023-09-26-new-UMLDocumentor/bg-post.jpg'
authors:
- NicolasAnquetil
comments: true
tags:
- Famix-tools
---

*This post is part of a serie dedicated to* Famix Tools

Once we have a model of a program in Famix, we often find ourselves wanting to ¨ go through it" to apply some systematic analysis or modification.
For example one could want to export the model as source-code [https://github.com/moosetechnology/FAMIX2Java](https://github.com/moosetechnology/FAMIX2Java).

The [Visitor design pattern](https://en.wikipedia.org/wiki/Visitor_pattern) is well adapted for these tasks.
Note that the [double-dispatch mechanism](https://en.wikipedia.org/wiki/Double_dispatch), which is an integral part of the visitor pattern, may also be useful to have entity specific actions even if one does not want to visit the entire model.

The Visitor pattern requires:
- an `accept: aVisitor` method in every entity of the meta-model (eg.: in FamixJavaClass, FamixJavaMethod, FamixJavaAttribute,...)
- `visitFamixXYZ: aFamixXYZ` for all entites to be visited, in the visitor class
- the `accept:` methods invoke the correct `visitFamixXYZ:` method of the visitor depending on the class it is implemented in
- the `visitFamixXYZ:` by default recursively visits all the "children" of the entity being visited (eg.: `visitFamixJavaClass:` should trigger the visit of the attributes and methods of the class)

For large meta-models, this can be cumbersome to implement as there may be many kinds of entities (43 for FamixJava) and the work is very repetitive.
The tool **FamixVisitorCodeGenerator** can do all the work for you, automatically.

## FamixVisitorCodeGenerator

Taking advantage of the meta-description of the Famix entities and the reflective nature of Pharo, it generates the `accept:` and  `visitFamixXYZ:` for all entities of a meta-model.

Usage exmaple:
```smalltalk
FamixVisitorCodeGenerator new
	package: 'Famix-Java-Entities' visitorClass: FamixJavaVisitor .
```

or for a FAST meta-model:
```smalltalk
FamixVisitorCodeGenerator new
	package: 'FAST-Java-Entities' visitorClass: FASTJavaVisitor.
```

The tool needs an empty visitor class (or trait) created by the user (`FamixJavaVisitor` in the example above), and a Pharo package containing the Famix classes of a meta-model (`Famix-Java-Entities` in the example above).
From this it will:
- create an  `accept:` method in all the classes in the given Famix package;
- the  `accept:` methods are created as extensions made by the package of the visitor;
- the `accept:` methods invoke the correct  `visitFamixXYZ:` depending on the class they are implemented in.
- a setter method allows to skip this part: `generateAccepts: false`
- the  `visitFamixXYZ:` methods are created in the visitor class (or trait) for a "maximal visit" (see below).

## Visiting methods

For a friendlier visitor, it is convenient that the visitor methods reproduce the inheritance hierarchy of the entities.
For example, if `FamixJavaAttribute` and `FamixJavaParameter` both inherit from  `FamixJavaVariable` entity, it is convenient that `visitFamixJavaAttribute:` and `visitFamixJavaParameter:` both call `visitFamixJavaVariable:` so that a common behaviour for the visit can be implemented only once.

Since Famix heavily relies on traits to compose meta-models, the same idea applies to used traits.
For example many entities implements `FamixTSourceEntity` to  get a `sourceAnchor`.
For all these entities, and if we need a generic behavior based on the source anchor, it is convenient that all the `visitXYZ:` methods call `visitTSourceEntity:` where the common behavior can be implemented.

Finally it might be convenient that the `visitXYZ:` methods, visit all the entites related to the one we are visiting.
For example when visiting a `FamixJavaVariable`, it might be interesting to recursively visit its `declaredType`.

All these conditions defines what we call a "maxium" visit of the meta-model.

## "Maximum" Visit

As described above, the maximum  `visitXYZ:` methods will trigger a resursive visit of many things (super-class visit method, used traits visit method, all entities related to the one being visited).

One down side of this is that a maximum visit is not a viable one in Famix because all relationships are bi-directionnal, causing infinite loops in the visit.
For example, if in a  `FamixJavaVariable` we recursively visit its `declaredType`, then in the `FamixJavaTType` we will recursively visit the `typedEntities` including the one we just came from.

There could be various solution to this problem:
- implement a memory mechanism in the visitor to remember what entities were already visited and do not go back to them.
- do not visit all relationships of an entity, but only its "children" relationship
- let the user handle the problem

For now the tool rely on the third solution.
If an actual visitor inherits (or use) the generated visitor, it must take care or redefining the visit methods so that it does not enter in an infinite loop (implementing any of the two other solutions)

In this sense, the maximum visit methods can be viewed as cheat sheets, showing all the things that one could do when visiting an entity.

In the future we might implement the second solution (visit only children) as an option of the visitor.
For now it is important to remember that **the generated visitor cannot be used as is**.
Methods must be redefined to get a viable visitor.

## class vs. trait visitor

The natural action would be to define the visitor as aclass from which all required actual visitors will inherit.
However, because visitors are so handy to go through the entire model, we discovered that we needed a lot of them (eg. visitors to create a copy of a model, to re-export a model) and they sometimes need to inherit from some other class.

As such, we rather recommend to create the maximal visitor as a trait that can be used by all actual visitors.
This make no difference for the `FamixVisitorCodeGenerator` and it might prove very useful.
