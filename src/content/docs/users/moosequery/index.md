---
authors:
- CyrilFerlicot
title: "Moose Query user documentation"
subtitle: "Documentation about Moose Query, the query feature of Metamodels described by Fame."
---

<!---
- Add comment about the future of Moose Query
- Add doc on the DSL
- Add doc on the objects
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

This navigation is divided into two parts:
- A set of simple methods to get the parents/children of an entity 
- A system of scoping query to navigate the different scopes of our containment DAG

### Simple navigation

Navigating the containment tree is the easiest way to query a model. It is possible with the containment properties in the description of the moose model. To do this, there is a really simple API on entities: 

| Selector     | Description                                           |
|--------------|-------------------------------------------------------|
| `#children`    | Direct children of the entity                         |
| `#parents`     | Direct parents of the entity                          |
| `#allChildren` | Children of the entity and their children recursively |
| `#allParents`  | Parents of the entity and their parents recursively   |

![A schema of a containement tree/DAG.](img/containmentTreeUser.png)
Example of a containment tree.

For example, the model in the example figure can be requested as follows:

**Examples of Containment Tree Navigation**

```smalltalk
package1 children. "=> { package2 . class1 }"
class3 children. "=> { attribute1 . attribute2 }"

package1 allChildren. "=> { package2 . class1 . class2 . class3 . attribute1 . attribute2 }"
class3 allChildren. "=> { attribute1 . attribute2 }"

package1 parents. "=> { }"
class3 parents. "=> { package2 }"
class4 parents. "=> { package3 . namespace1 }"

class3 allParents. "=> { package2 . package1 }"
attribute1 allParents. "=> { class3 . package2 . package1 }"
```

### Scoping Queries

TODO

## Navigating Dependencies (Associations)






