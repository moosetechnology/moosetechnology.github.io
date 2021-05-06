---
layout: page
background: '/img/bg-wiki.jpg'
title: "Typical Queries"
---

This page aims to present different queries one might want to perform when analyzing a model.
It can be used as a baseline for further analysis.

We present how to perform the queries in a playground or with the visual tool proposed by Moose.

- [Queries](#queries)
  - [God classes](#god-classes)
  - [God cyclomatic complexity](#god-cyclomatic-complexity)
  - [Deprecated methods](#deprecated-methods)
  - [Dead methods](#dead-methods)
  - [Application tests](#application-tests)
- [Visualizations](#visualizations)
  - [Class hierarchy](#class-hierarchy)
  - [Packages cycles](#packages-cycles)
- [Developers](#developers)
  - [Meta-model UML](#meta-model-uml)

## Queries

### God classes

[God classes](https://en.wikipedia.org/wiki/God_object) are classes that *knows too much* or *does too much*.
One way to find god classes is to count the number of lines or the number of methods of a class.

Using a playground, one can perform query on a model to retrieve the god classes by number of lines:

```st
model allModelClasses select: [ :each | each numberOfLinesOfCode > 150 ]
```

Or by number of methods:

```st
model allModelClasses select: [ :each | each numberOfMethods > 50 ]
```

### God cyclomatic complexity

The [cyclomatic complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity) presents the complixity of a program.
It basically corresponds to the number of possible branch in a method.
The less cyclomatic complexity for a method the better it is.

The following script sort the methods extracted in a model by their cyclomatic complexity.

```st
((self model allBehaviourals)
    collect: [ :entity | entity -> entity cyclomaticComplexity ]
    thenSelect: [ :assoc | assoc value > 3 ]) asOrderedCollection sort: #value descending
```

### Deprecated methods

Deprecated methods are methods existing in the current version of an application, and that will disapear in the next version.
Such method should not be used.

In Java, it is possible to retrieve the deprecated methods by searching the method with the `@Deprecated` annotation.

```st
model allModelMethods
    select: [ :t | 
        t annotationInstances notEmpty
            and: [ t annotationTypes anySatisfy: [ :a | a name = 'Deprecated' ] ] ]
```

### Dead methods

Dead methods are method that are not invoked by any other methods.

> Be carefull, some methods might not be directly call but are referenced thanks to abstraction or are dedicated to be extended by another application.
> It is often the case when dealing with framework.

```st
model allModelMethods
    collect: [ :method | method -> method incomingInvocations ]
    thenSelect: [ :assoc | assoc value isEmpty ]) asOrderedCollection
```

### Application tests

```st
^ (model allModelMethods
    select: [ :t | 
        t annotationInstances notEmpty
            and: [ t annotationTypes anySatisfy: [ :annotation | annotation name endsWith: 'Test' ] ] ])
    asOrderedCollection flattened
```

## Visualizations

### Class hierarchy

The class hierarchy present the classes of a model with their hierarchy links (*e.g.* superclass, subclasses, ...).
It also includes the methods and attributes of each class.

![Moose 9 version](https://img.shields.io/badge/Moose-9-%23aac9ff.svg){: .no-lightense }

```st
builder := RSUMLClassBuilder new.
classes := mooseModel allModelClasses.
builder modelDescriptor
        instVars: [ :aFamixClass | aFamixClass attributes ];
        instVarSelector: [:aFamixAttribute | aFamixAttribute name];
        methods: [ :aFamixClass | aFamixClass methods];
        methodSelector: [:aFamixMethod | aFamixMethod name].

builder classes: classes.
builder build.
builder canvas @ RSHierarchyPacker.
builder canvas
```

### Packages cycles

![Moose 8 version](https://img.shields.io/badge/Moose-8-%23aac9ff.svg){: .no-lightense }

```st
tarjan := MalTarjan new.
tarjan nodes: model allModelNamespaces.
tarjan edges: model allModelNamespaces from: #yourself toAll: [ :a | a allProvidersAtScope: FamixTNamespace ].
tarjan run.
tarjan inspect
```

## Developers

### Meta-model UML

It is possible to visualize the meta-model with a class hierarchy.

![Moose 9 version](https://img.shields.io/badge/Moose-9-%23aac9ff.svg){: .no-lightense }

```st
builder := RSUMLClassBuilder new.

classes := FamixJavaModel metamodel classes.

builder modelDescriptor
        instVars: [ :metaDescription | metaDescription primitiveProperties ];
        instVarSelector: [:metaDescription | metaDescription implementingSelector];
        methods: [ :metaDescription | metaDescription complexProperties];
        methodSelector: [:metaDescription | metaDescription implementingSelector].

builder classes: classes.

builder build.
builder canvas @ RSHierarchyPacker.
builder canvas
```
