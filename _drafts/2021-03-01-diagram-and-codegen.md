---
layout: post
title: "Model your Fame/Famix Metamodel using Graphical Editors"
subtitle: "Generate your Generator"
date: 2021-02-18 16:38:00 -0400
background: '/img/posts/bg-posts.jpg'
author: Vincent Aranega
---

Building metamodels for Fame and Famix is currently done through a generator.
The generator consists in various special methods that needs to be implemented, _eg_, one that will create all metaclasses, another that will create all attributes for each metaclasses,...
The metamodel's artifact are created programatically using pure Smalltalk syntax extended with specific operators to easily express relationships and cardinalities (`<>-`, `*-*`, `<>-*`, ...).
For more details, please refer to the [article about "Coaster"]({% post_url 2021-02-04-Coasters %}), written by Benoît Verhaeghe.

In this blogpost, I will show you how to use existing metamodeling diagraming tools to design your metamodel, and then to use the designed metamodel to generate your metamodel code for Moose (for Famix or Fame).


## Why a Graphical Modeling Editor

The generator concept provided for Fame/Famix in Moose is practical, but can be intimidating in the first place.
Moreover, discovering an existing metamodel, or modifying an existing one can be hard as it requires to switch between various methods.
Some projects exists, as [PlantUMLPharoGizmo](https://github.com/fuhrmanator/PlantUMLPharoGizmo), to automatically build a graphical representation of an existing metamodel.
This approach considers the code of the metamodel as main artifact and build the diagram out of it.

In this post, we will consider the opposite: puting the diagram as main artifact, then using the diagram as basis for generating the code of the Fame/Famix generator.
Despite its possible possible complexity to describe a generator for a full huge metamodel, the syntax for building the generator is easy to deal with and easy to generate.
The general process is sketched in Figure 1.
From the `graphical modeling editor`, the produced metamodel is given to a
`dedicated code generator` that will produce the `Fame/Famix Generator`.

!["General Flow, from Graphical Model to Fame/Famix Generator Code"](/img/posts/2021-03-01-diagram-and-codegen/general-flow.png){: .img-fill }


## Reusing Existing Modeling Editor

Here is an interesting fact.
Building a graphical modeling editor from scratch is hard.
It can looks easy in the first place, basically, it's only links and boxes, but it turns out that creating an editor that allows you to create a model, enforcings rules defined by a metamodel, is a complex task (without speaking about all graphical rules that need to be injected in the editor).
Consequently, we will reuse existing graphical modeling tools that already allows model metamodels.

Currently, there is no pure metamodeling editors for Pharo/Moose.
The only existing project is the [OpenPonk project](https://openponk.org/) which gives the ability to create UML models with XMI support, but UML proposes a lot of concepts which could lead to wrong design in the end (using wrong concepts).
On the other end, on the MDE community, [Ecore](https://www.eclipse.org/modeling/emf/) is an implementation of the [EMOF](https://www.omg.org/spec/MOF/2.4.1/PDF) and various desktop or web-based graphical editors exists.
Following then the flow we described earlier, the `Graphical Modeling Editor` will be one of the various graphical Ecore modeling editor.
The designed metamodel will be serialized in XMI (default output for those kind of editors), given to a code generator that will produce the Fame/Famix code generator.
The output of the code generator will follow the [tonel format](https://github.com/pharo-vcs/tonel) so it can be easily imported by Moose/Pharo.

### Aligning Ecore and Fame

As Fame, Ecore is a meta-metamodel that is used to create metamodels.
Various graphical tools exists for it.
The entry point of our chain is a 


## Design a Metamodel for Fame


## Design a Metamodel for Famix


## Generate your Generator


## Limitations
