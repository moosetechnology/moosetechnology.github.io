---
authors:
- CyrilFerlicot
title: "Pro tips"
subtitle: "List of developer knowledge to master Moose"
---

Moose is a big platform and hoards a lot of secrets to help developers work with it. The goal of this page is to discribe some of this knowledge. 

<!-- TOC -->

- [Attributes and Cache](#attributes-and-cache)
  - [Attributes](#attributes)
  - [Caches](#caches)

<!-- /TOC -->

## Attributes and Cache

Moose models can be really complexe and it happens a lot that we need a specific information that are not planned in the variables of our objects. In order to make it easier to save datas, we have two systems:
- attributes: allow to save some info that should never be flushed
- caches: allow to cache some info for performance reason

All subclasses of `MooseObject` are comming with those two mecanisms.

> In Moose 13 the names are "Attribute" and "Cache". We are planning to rename "Attribute" into "Property" in Moose 14, but we cannot do it now because the name "property" was used for another concept.

### Attributes

The typical usecase of attributes is to save information we cannot compute in Moose. For example, if you have a tool producing metrics and you want to manipulate them in your moose model you could implement it like this:

```smalltalk
myMetric
	^ self attributeAt: #myMetric ifAbsent: [  self notExistentMetricValue ]
```

and

```smalltalk
myMetric: aValue
    ^ self attributeAt: #myMetric put: aValue
```

The info added to attributes are not flushable. 
If you declare new Moose properties like we did here, be careful to not add the `<derived>` pragma in order to export the value of this property in JSON files:

```smalltalk
myMetric
	<FMProperty: #myMetric type: #Number>
	<FMComment: 'Description'>

	^ self attributeAt: #myMetric ifAbsent: [  self notExistentMetricValue ]
```

### Caches

The typical usecase of caches are to speed up some features by caching their values.

For example:

```smalltalk
numberOfMethods
	^ self cacheAt: #numberOfMethods ifAbsentPut: [ self types inject: 0 into: [ :sum :each | sum + each numberOfMethods ] ]
```
 and 

```smalltalk
numberOfMethods: aNumber
	self cacheAt: #numberOfMethods put: aNumber
```

Be careful to not save info you cannot recompute in this cache because it can be flushed by calling `flush` on the instance of your moose object.
Also, if you declare a property, don't forget to add the `<derived>` pragma to not export this value that can be recomputed.

```smalltalk
numberOfMethods

	<FMProperty: #numberOfMethods type: #Number>
	<FMComment: 'The number of methods in a package'>
	<derived>
	^ self cacheAt: #numberOfMethods ifAbsentPut: [ self types inject: 0 into: [ :sum :each | sum + each numberOfMethods ] ]
```