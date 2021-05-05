---
layout: page
background: '/img/bg-wiki.jpg'
title: "Typical Queries"
---

This page aims to present different queries one might want to perform when analyzing a model.
It can be used as a baseline for further analysis.

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

By lines:

```st
model allModelClasses select: [ :each | each numberOfLinesOfCode > 150 ]
```

By method:

```st
model allModelClasses select: [ :each | each numberOfMethods > 50 ]
```

### Deprecated methods

```st
model allModelMethods
    select: [ :t | 
        t annotationInstances notEmpty
            and: [ t annotationTypes anySatisfy: [ :a | a name = 'Deprecated' ] ] ]
```

### Dead methods

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
