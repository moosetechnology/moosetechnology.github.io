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
  - [Deprecated methods](#deprecated-methods)
  - [Dead methods](#dead-methods)
  - [Application tests](#application-tests)
- [Visualizations](#visualizations)
  - [Packages cycles](#packages-cycles)
- [Sources](#sources)

## Queries

### God classes

God classes are classes that *knows too much* or *does too much*.
One way to find god classes is to count the number of lines or the number of methods of a class.

Using a playground, one can perform query on a model to retrieve the god classes by number of lines:

```st
model allModelClasses select: [ :each | each numberOfLinesOfCode > 150 ]
```

Or by number of methods:

```st
model allModelClasses select: [ :each | each numberOfMethods > 50 ]
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

### Packages cycles

![Moose version](https://img.shields.io/badge/Moose-8-%23aac9ff.svg){: .no-lightense }

```st
tarjan := MalTarjan new.
tarjan nodes: model allModelNamespaces.
tarjan edges: model allModelNamespaces from: #yourself toAll: [ :a | a allProvidersAtScope: FamixTNamespace ].
tarjan run.
tarjan inspect
```

## Sources

The queries presented here were first presented in:

- [Benoît Verhaeghe's website](https://badetitou.github.io/misc/moose/pharo/2019/09/13/OOAnalysis/)
