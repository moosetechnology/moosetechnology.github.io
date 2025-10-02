---
authors:
- CyrilFerlicot
title: "Moose Query user documentation"
subtitle: "Documentation about Moose Query, the query feature of Metamodels described by Fame."
---

<!-- TOC -->

- [Introduction](#introduction)
- [Exploring a Containment Tree](#exploring-a-containment-tree)
  - [Moose Query containment DSL](#moose-query-containment-dsl)
    - [Select the direction of the query](#select-the-direction-of-the-query)
    - [Containment query options](#containment-query-options)
    - [Execute the containment query](#execute-the-containment-query)
  - [Containment syntactic sugar](#containment-syntactic-sugar)
- [Navigating Dependencies (Associations)](#navigating-dependencies-associations)

<!-- /TOC -->


<!---
- Add comment about the future of Moose Query
- Add doc on the DSL
- Add doc on the objects
- Add doc on query result
-->

Moose-Query is a domain-specific language to build navigations and scopes queries for entities in Moose. Its authors are Anne Etien, Jean-Christophe Bach, Vincent Blondeau and Cyril Ferlicot-Delbecque.

This documentation is up to date with Moose 13 alpha version of the 29th September 2025.

> Note: A documentation for Moose 6.1 is available at: [https://moosequery.ferlicot.fr](https://moosequery.ferlicot.fr).

## Introduction

Moose-Query is an internal domain-specific language (DSL) to build queries for entities in Moose. It replaces Moose-Chef, and is designed to simplify and standardize the way to query Moose models.

In order to visualize the concepts, let's take an example of a simple application model:
![Image representing a small model with containment and associations examples.](img/moose-graph.png)


An application model in Moose is composed of two concepts:

**Nesting of entities**
    
It defines which entity contains which entities. For example, it defines the classes contained in a package and the methods contained in a class. In the example image, it is the relations `parentPackage`, `container`, and `parentType`. This concept allows one to build the containment tree of a model. 

**Associations between entities**

It specifies how the entities interact with each other. For example, an inheritance is an association between a `subclass` and its `superclass`. Another example is that a reference is an association between a `behavioral entity` and a `type`. In the example image, it can be the `:Inheritance` with the `superclass` and `subclass` relations. In the case of software analysis, we can see the association as the reification of dependencies in an application.

MooseQuery allows exploring a model via those two concepts. Then, you can:
* Explore the containment tree (or DAG to be more precise)
* Gather associations
* Manipulate the gathered associations
* Change the scope of entities (move from classes to the parent package or the children methods)

In this documentation, we detail the use of the DSL. With this tool, you will be able to query your Moose model, for example, to:
* Get the children of an entity (Exploring a containment tree)
* Get all the associations contained in a class (Navigating associations)
* Get all the entities who depend on a specific entity (Manipulating the gathered associations)
* Get all the methods contained in a package in a Java model, including those in inner/anonymous classes (Changing scope)

In order for the query system to work, we need to have a model with a meta description (that we cann acces via the method #`mooseDescription`) that has the right `container`, `source` and `target` properties. In order to set those properties, you can check the documentation on 

## Exploring a Containment Tree

It is possible to navigate the containment DAG of a model easily with Moose Query. 

This documentation will be divided into two parts:
- An explanation of the MooseQuery DSL to create queries
- An explanation of the syntactic suggar we have to cover the most common usecases of scoping queries

### Moose Query containment DSL

The class `MooseQuery` is the entry point of the query system of Moose. We are able to query any object using the trait `TEntityMetalevelDependency`.

Queries should start my sending `query` to an entity. For example: 

```smalltalk
method query
```

Then they are composed of 3 parts:
- A message to initialize a scope or navigation query
- A list of options
- A final message to execute the query

#### Select the direction of the query

Containment queries have 2 different messages to initialize them.

```smalltalk
	method query containers.
	method query containedEntities.
```

In case you wish to explore the containers of your entity, you can initialize the query with `#container`. But if you wish to explore the children of your entity, you can use `#containedEntities`.

#### Containment query options

Then the query has different parameters (optional):

```smalltalk
	method query containers recursively. 
	method query containers until: #isClass.
```

The method `#recursively` indicates that query will not stop at the first result found but proceed in checking for more results higher and lower in the containment. Withtout the option, we stop at the first containers/containedEntities we find."

For example, in a java application you can do:

```smalltalk
    method query containers ofType: FamixTPackage
```

This will give you first entity using `FamixTPackage` containing the class containing the receiver method.

But it is possible that we want to collect also the packages containing this package recursively. In that case we can do:

```smalltalk
    method query containers recursively ofType: FamixTPackage
```

The method `#until:` allows you to add extra conditions to finish a query. This can be used in different ways like speeding up the query by cutting lookup branches or to add some behavior.

For example we could want to stop the query if we find entities we already visited:

```smalltalk
    (entity query containers recursively until: [ :e | (potentialDependencies includes: e) or: [ dependencies includes: e ] ]) ofAnyType
```

#### Execute the containment query

Last, you can have a parameter that will finish to configure and execute the query:

```smalltalk
	method query containers ofType: FamixTClass. "ofType: will select the containers/containedEntities matching the kind in parameter."
	method query containers ofAnyType: { FamixTClass . FamixTNamespace }. "ofAnyTypes: will select the containers/containedEntities matching any kind in parameter."
	method query containers ofAnyType. "Select all containers independently of the type."
    method query containers withProperty: #hasSourceAnchor.
```

Wihle sending one of those, the query will be directly executed. 

- `#ofType:` will select the containers/containedEntities matching the kind in parameter. It can be a class (`FamixJavaMethod`) or a trait used by the concerned entity (`FamixTMethod`).
- `#ofAnyTpe:` will work in the same way, but takes a colletion of types instead of a single type.
- `#ofAnyType` will select everything independently of its type. This option is only useful if the option `#recursively` is active.
- `#withProperty:` will not stop the query depending on the type of the entities but depending on the result of the block provided as parameter.

For example if you want to find the first package in your hierarchy you can do:

```smalltalk
    method containers ofType: FamixTPackage
```

Now let's imagine we are in Python and we want all the functions and methods of a module:

```smalltalk
    module containedEntities ofAnyType: { FamixTMethod . FamixTFunction }
```

Now let's collect all the containers recursively of a method:

```smalltalk
    method containers recursively ofAnyType
```

Finaly let's imagine we want to stop on a condition that is not the type of the collected entity, we can do it with `#withProperty:`:

```smalltalk
    method containers withProperty: #isInterface.
    
    method containers withProperty: [ :object | object isType and: [ object typeContainer isPackage ] ].
```

More random examples:

```smalltalk
	method := model allModelMethods anyOne.

	method query containers ofType: FamixTNamespace.
	method query containers recursively ofType: FamixTNamespace.
	method query containers recursively ofAnyType: {FamixTNamespace . FamixTClass}.
	(method query containers recursively until: #isClass) ofAnyType: {FamixTNamespace . FamixTClass}.

	method query containedEntities recursively ofType: FamixTNamespace.
	(method query containedEntities recursively until: #isClass) ofAnyType: {FamixTNamespace . FamixTClass}.
```

### Containment syntactic sugar

For the most common usecases we added some syntactic suggar on `TEntityMetalevelDependency`:

| Selector     | Description                                           |
|--------------|-------------------------------------------------------|
| `#containedEntities`    | Direct contained entities of the receiver                         |
| `#containers`     | Direct containers of the receiver                          |
| `#allContainedEntities` | Contained entities of the receiver and their contained entities recursively |
| `#allContainers`  | Containers of the receiver and their containers recursively   |
| `#containedEntitiesOfType:` | Equivalent of `x query containedEntities ofType:` |
| `#containersOfType:` | Equivalent of `x query containers ofType:` |
| `#allContainedEntitiesOfType:` | Equivalent of `x query containedEntities recursively ofType:` |
| `#allContainersOfType:` | Equivalent of `x query containers recursively ofType:` |

Example of a containment tree:
![A schema of a containement tree/DAG.](img/containmentTreeUser.png)

For example, the model in the example figure can be requested as follows:

**Examples of Containment Tree Navigation**

```smalltalk
package1 containedEntities. "=> { package2 . class1 }"
class3 containedEntities. "=> { attribute1 . attribute2 }"

package1 allContainedEntities. "=> { package2 . class1 . class2 . class3 . attribute1 . attribute2 }"
class3 allContainedEntities. "=> { attribute1 . attribute2 }"

package1 containers. "=> { }"
class3 containers. "=> { package2 }"
class4 containers. "=> { package3 . namespace1 }"

class3 allContainers. "=> { package2 . package1 }"
attribute1 allContainers. "=> { class3 . package2 . package1 }"
```

## Navigating Dependencies (Associations)






