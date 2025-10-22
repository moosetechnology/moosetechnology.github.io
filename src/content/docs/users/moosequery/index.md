---
authors:
- CyrilFerlicot
title: "Moose Query user documentation"
subtitle: "Documentation about Moose Query, the query feature of Metamodels described by Fame."
---

<!-- TOC -->
- [Introduction](#introduction)
  - [Relations between entities in Moose](#relations-between-entities-in-moose)
    - [Containment](#containment)
    - [Associations between entities](#associations-between-entities)
  - [Navigating relations](#navigating-relations)
- [Exploring a Containment Tree](#exploring-a-containment-tree)
  - [Moose Query containment DSL](#moose-query-containment-dsl)
    - [Initialize a containment query by choosing a direction](#initialize-a-containment-query-by-choosing-a-direction)
    - [Containment query options](#containment-query-options)
      - [Recursive queries](#recursive-queries)
      - [Stop condition](#stop-condition)
    - [Execute the containment query](#execute-the-containment-query)
  - [Containment syntactic sugar](#containment-syntactic-sugar)
- [Navigating Dependencies (Associations)](#navigating-dependencies-associations)
  - [Moose Query navigation DSL](#moose-query-navigation-dsl)
    - [Initialize the navigation query](#initialize-the-navigation-query)
    - [Add options to the navigation query](#add-options-to-the-navigation-query)
      - [Get only local dependencies](#get-only-local-dependencies)
      - [Receive entities instead of associations](#receive-entities-instead-of-associations)
    - [Execute the navigation query](#execute-the-navigation-query)
  - [Navigation syntactic suggar](#navigation-syntactic-suggar)
  - [Manipulating the gathered results of a navigation query](#manipulating-the-gathered-results-of-a-navigation-query)
- [Future plans for MooseQuery](#future-plans-for-moosequery)

<!-- /TOC -->

Moose-Query is a domain-specific language to build navigation and scope queries for entities in Moose.
Its authors are Anne Etien, Jean-Christophe Bach, Vincent Blondeau and Cyril Ferlicot-Delbecque.

This documentation is up-to-date with Moose 13 alpha version of the 6th October 2025.

:::note[A documentation for Moose 6.1 is available at: [https://moosequery.ferlicot.fr](https://moosequery.ferlicot.fr)]
:::

## Introduction

Moose-Query is an internal domain-specific language (DSL) for creating queries on the relations between entities in Moose. 
It replaces Moose-Chef, and is designed to simplify and standardize the way to query Moose models.

In order to visualize the concepts, let's take an example of a simple application model:
![Image representing a small model with containment and associations examples.](img/moose-graph.png)

### Relations between entities in Moose

An application model in Moose represents two types of relations:

#### Containment

It defines which entity contains or are contained in which entities.
For example, for a class, it defines the package it is contained in, and the methods and attributes it contains.
In the example image, it is the relations `parentPackage`, `container`, and `parentType`.
This concept allows one to build the containment tree of a model.

#### Associations between entities

It represents the dependencies between entities.
In an application model in Moose, associations are the reification of a dependency.
For example, an inheritance is an association between a `subclass` and its `superclass`.
In the example image, it can be the `:Inheritance` with the `superclass` and `subclass` relations.
Another example is that a reference is an association between a `behavioral entity` and a `type`.

### Navigating relations

MooseQuery allows exploring a model via those two concepts.
With this tool, it is possible to:

- Explore the containment tree (the containment DAG, to be more precise): get the entities containing/contained in an entity.
- Gather associations: get the dependencies of an entity.
- Manipulate the gathered associations: get the entities that an entity depends or is dependent on
- Change the scope of entities (move from container to contained entities and vice versa)

Moose Query relies on the model meta-description (that we can access via the method `#mooseDescription`).
The properties of the entities must be meta-described as `container`, `source` and `target`.
In order to set those properties, you can check the documentation on.

## Exploring a Containment Tree

:::caution[In reality we have a containment directed acyclic graph (DAG).]
An entity may be contained in several entities, for example in a package and in a file.
However, most entities have only one parent.
For simplicity, we will refer to it as a containment tree.
:::

It is possible to navigate the containment tree of a model easily with Moose Query.

This documentation will be divided into two parts:

- An explanation of the MooseQuery DSL to create queries
- An explanation of the syntactic sugar we have to cover the most common use cases of scoping queries

### Moose Query containment DSL

The `MooseQuery` class is the entry point of the query system of Moose.
We are able to query any object using the trait `TEntityMetalevelDependency`.

Queries should start my sending `query` to an entity.

```smalltalk
entity query
```

Then they are composed of 3 parts:

- A message to initialize a containment query
- A list of options
- A final message to execute the query

#### Initialize a containment query by choosing a direction

To initialize a containment query, two directions are available: `container` and `containedEntities`.
The direction choice is made by sending either of these messages:

```smalltalk
entity query containers.
entity query containedEntities.
```

:::note[Deprecation]
In Moose 12 and previous versions, the equivalent of `#containers` was `#ancestors` and the equivalent of `#containedEntities` was `#descendants`.
:::

#### Containment query options

A containment query can be parameterized to:

- Gather all containers recursively
- Gather containers recursively until a condition is matched.

```smalltalk
entity query containers recursively. 
entity query containers recursively until: #isClass.
```

##### Recursive queries

By default, a containment query stops at the first level, i.e. with the first found entities.
With the `#recursively` option, the query gathers the entities contained in the contained entities or the containers of the containers.

```smalltalk
entity query containers recursively
```

##### Stop condition

When using the `recursively` option, a stopping condition can be added.
The method `#until:` adds a condition that ends the query when met.
This can be used in different ways like speeding up the query by cutting lookup branches or to add some behavior.

For example, to stop the query if the entity were already visited:

```smalltalk
entity query containers recursively until: [ :e | self knownDependencies includes: e ]
```

#### Execute the containment query

To execute the query, several triggers exist:

- `#ofAnyType` - Any containers.
Will select containers regardless of their type.
- `#ofType:` - Containers of a specific type.
Will select the entities typed according to the type (class or trait) provided as parameter.
- `#ofAnyTpe:` - Containers of specific types.
Will select the entities typed according to any type (class or trait) in the collection provided as parameter.
- `#withProperty:` - Containers matching a property.
Will select the entities matching the condition provided as parameter.

```smalltalk
entity query containers ofAnyType.
entity query containers ofType: FamixTClass. 
entity query containers ofAnyType: { FamixTClass . FamixTNamespace }. 
entity query containers withProperty: #hasSourceAnchor.
```

Example of a containment tree to use for the next examples:
![A schema of a containement tree.](img/containmentTreeUser.png)

For example if you want to find the first package containing Class2:

```smalltalk
class2 query containers ofType: FamixTPackage "=> { package2 }"
```

Now let's imagine we have the package1 and we want to find all its children packages and Enum (even if in this particular case we have no enum inside):

```smalltalk
package1 query containedEntities ofAnyType: { FamixTPackage . FamixJavaEnum } "=> { package2 }"
```

We use `FamixJavaEnum` because we do not have a trait `FamixTEnum`, but MooseQuery is able to work with Traits and concrete entities. 

Now let's collect all the containers recursively of a method:

```smalltalk
attribute2 query containers recursively ofAnyType "=> { class3 . package2 . package1 }"
```

Finaly let's imagine we want to stop on a condition that is not the type of the collected entity, we can do it with `#withProperty:`:

```smalltalk
attribute2 query containers withProperty: #isPackage. "=> { package2 }"
    
attribute2 query containers withProperty: [ :object | object isType and: [ object typeContainer isPackage ] ]. "=> { class3 }"
```

The last example is able to exclude inner classes for example since we are looking for a class in a package.

More random examples:

```smalltalk
method := model allModelMethods anyOne.

entity query containers ofType: FamixTNamespace.
entity query containers recursively ofType: FamixTNamespace.
entity query containers recursively ofAnyType: {FamixTNamespace . FamixTClass}.
(entity query containers recursively until: #isClass) ofAnyType: {FamixTNamespace . FamixTClass}.

entity query containedEntities recursively ofType: FamixTNamespace.
(entity query containedEntities recursively until: #isClass) ofAnyType: {FamixTNamespace . FamixTClass}.
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

This API is the one from Moose 13. In the previous version of Moose the API was different:

| Selector in Moose 13    | Selector in Moose < 13                                           |
|--------------|-------------------------------------------------------|
| `#containedEntities`    | `#children` |
| `#containers`     | `#parents` |
| `#allContainedEntities` | `#allChildren` |
| `#allContainers`  | `allParents` |
| `#containedEntitiesOfType:` | `#toScope:` |
| `#containersOfType:` | `#atScope:` |
| `#allContainedEntitiesOfType:` | `#allToScope:` |
| `#allContainersOfType:` | `#allAtScope:` |

For the following examples, let's use the same model as the previous section:
![A schema of a containement tree.](img/containmentTreeUser.png)

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

It is possible to navigate the dependencies of a model easily with Moose Query. 

This documentation will be divided into two parts on the same schema than the containment queries:
- An explanation of the MooseQuery DSL to create queries
- An explanation of the syntactic suggar we have to cover the most common usecases of navigations queries

### Moose Query navigation DSL

The `MooseQuery` DSL for navigation queries works the same way than the containement queries DSL. The class `MooseQuery` is the entry point of the query system of Moose and we are able to query any object using the trait `TEntityMetalevelDependency`.

Queries should start my sending `query` to an entity. For example: 

```smalltalk
entity query
```

Then they are composed of 3 parts:
- A message to initialize a navigation query
- A list of options
- A final message to execute the query

#### Initialize the navigation query

Navigation queries have 3 different messages to initialize them:

```smalltalk
entity query has.
entity query incoming.
entity query outgoing.
```

- `#incoming` allows one to query the incomming dependencies of an entity. This means that it will query entities who depend on the receiver. For example, if the receiver is a method, it will query the entities using this method.
- `#outgoing` allows one to query the outgoing dependencies of an entity. This means that it will query entities used by the receiver. For example, if the receiver is a method, it will query the entities used in the definition of the method.
- `#has` is a special starter that will check if dependencies to an entity exists or if there are none. It should have a direction as a complement and exists only for performance reasons (as it can stop as soon as it found 1 result). It is useful for example to find statically dead entities.

If you are using `#has` you should use it this way:

```smalltalk
entity query has incoming. 	"Define that the query will check the existance of incoming dependencies."
entity query has outgoing. 	"Define that the query will check the existance of outgoing dependencies."
```

#### Add options to the navigation query

As the containement queries, the navigations queries can have options:

```smalltalk
entity query incoming local.
entity query incoming objects.
```

##### Get only local dependencies

By default if you ask the dependencies of an entity, you will get the dependencies of the entity and its children. For example, if you ask for the dependencies of a class, you will also get the dependencies of its methods. 

In some rare cases, we might want the local dependencies of the entity without looking in the children. In that case we can use the `#local` option:

```smalltalk
entity query incoming local dependenciesOfType: FamixTAccess
```

##### Receive entities instead of associations

By default the queries will return a collection of associations (if the parameters #has is not passed, in that case it will be a boolean). You can change this to collect the entities at the opposite of the association instead:

```smalltalk
entity query incoming objects dependenciesOfType: FamixTAccess
```

#### Execute the navigation query

Last, you can have a parameter that will finish to configure and execute the query:

```smalltalk
entity query incoming dependencies.
entity query incoming dependenciesOfType: FamixTReference.
entity query incoming dependenciesOfAnyType: { FamixTReference . FamixTInvocation }.
```

Wihle sending one of those, the query will be directly executed. 

- `#dependenciesOfType:` will select the dependencies corresponding to the the kind in parameter. It can be a class (`FamixJavaAccess`) or a trait used by the concerned entity (`FamixTAccess`).
- `#dependenciesOfAnyType:` will work in the same way, but takes a colletion of types instead of a single type.
- `#dependencies` will select all dependencies independently of its type.

Example of a navigation model to use for the next examples:
![A schema of a navigation model.](img/navigationUser.png)

For example, let's find all incoming inheritances of Class1:

```smalltalk
class1 query incoming dependenciesOfType: FamixTInheritance. "=> { inheritance1 }"
class1 query incoming object dependenciesOfType: FamixTInheritance. "=> { class2 }"
```

Now let's find the incoming inheritances and references of class1:

```smalltalk
class1 query incoming dependenciesOfAnyType: { FamixTInheritance . FamixTReference }. "=> { inheritance1 . reference1 }"
class1 query incoming object dependenciesOfType: FamixTInheritance. "=> { class2 . method2 }"
```

In the next example we will ask for all dependencies of class 1 independently of their kind.

```smalltalk
class1 query incoming dependencies. "=> { inheritance1 . reference1 }"
class1 query incoming object dependencies. "=> { class2 . method2 }"
class1 query outgoing dependencies. "=> { access1 } This is here because Class1 contains Method1 that access Attribute1 and queries are recursive by default"
class1 query outgoing object dependencies. "=> { attribute1 }"
```

As we have seen, asking the outgoing dependencies of `Class1` gives `Attribute1` because it is accessed in `Method1` that is contained in `Class1`. But it is possible to restrain this:

```smalltalk
class1 query outgoing local dependenciesOfType: FamixTAccess. "=> { }"
method1 query outgoing local dependenciesOfType: FamixTAccess. "=> {access1 }"
```

Other examples of local queries:

```smalltalk
class1 query incoming local dependencies. "=> { inheritance1 . reference1 }"
class1 query outgoing local dependencies. "=> { }"
```

More random examples:

```smalltalk
method := model allModelMethods anyOne.

entity query incoming dependencies.
entity query incoming dependenciesOfType: FamixTReference.
entity query incoming local dependenciesOfType: FamixTReference.
entity query incoming local object dependenciesOfType: FamixTReference.
entity query incoming local object dependenciesOfAnyType: { FamixTReference . FamixTInvocation }.

entity query outgoing local object dependenciesOfType: FamixTReference.
	
entity query has incoming local object dependenciesOfAnyType: { FamixTReference . FamixTInvocation }.
```

### Navigation syntactic suggar

For the most common usecases we added some syntactic suggar on `TEntityMetalevelDependency` like for containment queries:

| Selector        | Description |
|-----------------|-------------|
| `#query:with:`      | The first parameter is a symbol (`#out`or `#in`) and return all the instances of the association class defined by the second parameter  |
| `#queryAll:`        | The first parameter is a symbol (`#out`or `#in`)and return all the dependencies of this direction |
| `#queryAllIncoming` | Return all the incoming dependencies of this direction |
| `#queryAllOutgoing` | Return all the outgoing dependencies of this direction |
| `#queryIncoming:` | Return all the incoming dependencies if the kind provided as parameter |
| `#queryOuutgoing:` | Return all the outgoing dependencies if the kind provided as parameter |

For the following examples, let's use the same model as the previous section:
![A schema of a navigation model.](img/navigationUser.png)

```smalltalk

class1 query: #in with: FamixTInheritance. "=> { inheritance1 }"
class1 query: #out with: FamixTInheritance. "=> { }"
class1 query: #out with: FamixTAccess. "=> { access1 }. Method1 contained in Class1 access to Attribute1 via Access1, result of the query. The access is not directly done via Class1, but via its children."

class1 queryAll: #in. "=> { inheritance1 . reference1 }"
class1 queryAll: #out. "=> { access1 }"

class1 queryAllIncoming. "=> { inheritance1 . reference1 }"
class1 queryAllOutgoing. "=> { access1 } This is here because Class1 contains Method1 that access Attribute1 and queries are recursive by default"

class1 queryIncoming: FamixTInheritance. "=> { inheritance1 }"
class1 queryOutgoing: FamixTInheritance. "=> { }"
class1 queryOutgoing: FamixTAccess. "=> { access1 }. Method1 contained in Class1 access to Attribute1 via Access1, result of the query. The access is not directly done via Class1, but via its children."

```

### Manipulating the gathered results of a navigation query

A navigation query returns a result as a `MooseQueryResult`. There are three types of query results:
- `MooseIncomingQueryResult` manages the result of a query on incoming associations.
- `MooseOutgoingQueryResult` manages the result of a query on outgoing associations.
- `MooseObjectQueryResult` is special kind of query result that can be obtained via the two others and includes the opposites of the receiver associations. For example, if you query the outgoing accesses of a class, the opposites of the class associations will be the accessed attributes. It can also be obtained by using the `object` parameter of the navigation DSL.

These query result classes are special collections with some new features.

One of the most useful ones is the #opposites method present on `MooseIncomingQueryResult` and `MooseOutgoingQueryResult`. It returns a `MooseObjectQueryResult` containing all the opposite entities of the query result relative to the receiver of the query.

Indeed, when we query a model, it is often not the associations that we want as result but the source/target entities of these associations. For example, if we want the instance variables accessed by a given method, #query:with: will return the accesses, whereas sending the message #opposites on the result returns the variables themselves.

Taking the previous model as an example we can do:

**Opposite query example**

```smalltalk
class1 queryAll: #in. "=> { inheritance1 . reference1 }"
(class1 queryAll: #in) opposites. "=> { class2 . method2 }"
```                        

Another possibility is to exclude all the results where the opposite is included in the receiver of the original query. This can be done via the method `#withoutSelfLoop`. It is handy when we want to query a model to find external dependencies without internal ones.

For the next examples let's imagine that `Attribute1` in our example is contained by `Class1`.

```smalltalk
class1 query outgoing dependencies withoutSelfLoop "=> { } We don't have access1 anymore because it is in `Class1`." 
```

Another feature is to be able to change the scope of the result:

```smalltalk
class1 query outgoing dependencies containersOfType: FamixTClass "=> { class1 }" 
```

This will be the result because the query will target `Attribute1` that is contained in `Class1`.


## Future plans for MooseQuery

Currently Moose query is tight to Famix models. But in fact, it should only be coupled to Fame because all meta informations are coming from Fame and we should not depend on Famix.
Over the years we are reducing those dependencies little by little. The end goal is to have a `FameQuery` project that could work on any metamodel meta-described by Fame.

For this documentation, we could also add a section on the methods we can use to know the possible kind of containers, contained entities, sources and targets. 
