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

In this blogpost, I will show you how to use existing metamodeling diagraming tools to design your metamodel, and then to use the designed metamodel to generate your metamodel code for Fame in Moose.
In another blogpost, I will show you how to use this same generator to use graphical modeling tools to generate code for Famix.


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
Various graphical tools exists for it, so we have a little bit of choice.
The entry point of our chain is an Ecore metamodel in a XMI format and the destination a Smalltalk class in a tonel format, but, the generated class will describe a generator for a Fame metamodel, not an Ecore one.
Consequently, we need to allign Ecore concepts with Fame concepts.
This blogpost does not go in details about the full alignement between Ecore and Fame, but gives some keys thare are used.

#### Metaclasses, EClass and Traits

Fame proposes two main concepts: `FM3Class` (metaclasses) and `FM3Traits` (traits).
On the contrary, Ecore only provide the concept of `EClass` to represent metaclasses, which is aligned with the metaclass concept of Fame.
Also, Ecore let users mark a metaclass as `interface`, which is a concept that comes close to the trait concept in Fame.
Thus, The alignement that we will keep here is:

* EClass -> FM3Class (metaclasses)
* EClass interface -> FM3Traits (traits)

#### Attributes

Attributes in Fame are modeled by the concept of `FM3Property`.
Attributes represent a property of a metaclass/trait and are usually bound to objects outside of the Fame "ecosystem" (as `String` that comes from Pharo for example).
The same concept can be found in Ecore with the `EAttribute` concept.
On the contrary of attributes in Fame, `EAttribute` in Ecore can own default values (and a various set of other attributes that we will not consider here).
Consequently, we can directly align simple `EAttribute` to Fame attribute, ignoring the default value if one is set.
In this condition, the alignement is:

* EAttribute -> FM3Property

#### References

References in Fame are also modeled by the concept of `FM3Property`.
They can own a set of properties and can be marked as "containement", and they own a cardinality.
References represent relationships between metaclasses.
In Ecore, the `EReference` concept is the equivalent to `FM3Property`, that's then the one we use to model relationships.
The alignment is:

* EReferences -> FM3Property


#### Untackled Concepts from Ecore

Ecore propose more concepts which are not directly aligned with Fame, _e.g._: `EEnumeration`, `EGenericType`,...
Thus, those concepts are not handled by the generator.


## Designing a Metamodel for Fame

Now that we know how we will represent Fame concepts with Ecore, we can use a graphical modeling tool to design Ecore metamodel to design Fame metamodels.

For this blogpost, three tools have been tested, two web-based one and a desktop one:

* [GenMyModel](https://www.genmymodel.com/) (web-based)
* [Ecore Diagram](https://www.eclipse.org/ecoretools/doc/) (desktop)
* [EMF.cloud](https://www.eclipse.org/emfcloud/) (web-based)

They all work well and have their pros and cons.
As example/illustration of this section I used GenMyModel (I worked their for few years, I know well the tool), but the design process is the same whatever the used tool.

When you design your metamodel for Fame using a graphical Ecore editor, you just focus on the concepts you want to represent.
The metamodel we are going to design is the same presented by Benoît in his [article about "Coaster"]({% post_url 2021-02-04-Coasters %}).

The designed Ecore metamodel can be found [at this address](https://app.genmymodel.com/api/repository/longduzboub/Coaster) and it looks like this:

!["Coaster Metamodel in Ecore"](https://app.genmymodel.com/api/projects/_vgiYcHuVEeuCM8KqVoRWiA/diagrams/_vgiYcnuVEeuCM8KqVoRWiA/svg){: .img-fill }

In addition, we also set two specific properties of the base `EPackage`: `nsURI` and `nsPrefix`.
While the `nsURI` is more internal/maintenance related (it can be used to propose a unique identifier towards this version of the metamodel) `nsPrefix` is really important as it will set the prefix that will be used later for the generation of the generator.
I thus set the two values to:

* `nsURI` = `https://coaster/1.0`
* `nsPrefix` = `Ct`.

With this, we are ready to generate the code of our generator.

## Generate your Generator

The code generator is coded in Python using the PyEcore library to be able to decode/resolve Ecore files.
The generator go across the full metamodel at least once to perform model validation, tries to correct some issues (like spaces at the beginning or end or names) and raise an error if it cannot generate the code for some reasons.
Once the metamodel is validated, it generate the Fame generator code using [Mako](https://www.makotemplates.org/), a template engine.


### Install the code generator


### Generate the code of your Generator

If you did your metamodel online, there is two options for you.
Either you can download the `.ecore` on your machine and generate the code from there, or you can give to the generator a valid URL where the XMI of your ecore metamodel is located.
Whatever option you'll choose, you can generate the code of your generator this way:

```bash
$ python famegen.py <URL_OR_FILE> -o mymeta.class.st
```

This will generate the code of your generator in a tonel format in the file `mymeta.class.st`.
If you don't use the option `-o`, the generated code is displayed on standard output.

Here is the line used to generate the generator of the `Coaster` metamodel:

```bash
$ python famegen.py https://app.genmymodel.com/api/projects/_vgiYcHuVEeuCM8KqVoRWiA/xmi -o /tmp/coaster.class.st
```

Here is the content of the file:

```st
Class {
  #name : #CoasterGenerator,
  #superclass : #FamixMetamodelGenerator,
  #instVars : [
    'shape',
    'coaster',
    'creator',
    'country',
    'round',
    'square',
    'oval',
    'brewery'
  ],
  #category : #Coaster
}


{ #category : #accessing }
CoasterGenerator class >> packageName [
	^ 'Coaster'
]

{ #category : #accessing }
CoasterGenerator class >> prefix [
	^ 'Ct'
]


{ #category : #definition }
CoasterGenerator >> defineClasses [
  super defineClasses.

  shape := builder newClassNamed: #Shape.
  coaster := builder newClassNamed: #Coaster.
  creator := builder newClassNamed: #Creator.
  country := builder newClassNamed: #Country.
  round := builder newClassNamed: #Round.
  square := builder newClassNamed: #Square.
  oval := builder newClassNamed: #Oval.
  brewery := builder newClassNamed: #Brewery.
]

{ #category : #definition }
CoasterGenerator >> defineHierarchy [
  super defineHierarchy.

  round --|> shape.
  square --|> shape.
  oval --|> shape.
  brewery --|> creator.
]

{ #category : #definition }
CoasterGenerator >> defineProperties [
  super defineProperties.

  coaster property: #image type: #String.
  coaster property: #number type: #Number.
  coaster property: #owned type: #Boolean.
  creator property: #name type: #String.
  country property: #name type: #String.
]

{ #category : #definition }
CoasterGenerator >> defineRelations [
  super defineRelations.

   (shape property: #coasters)  *-* (coaster property: #shapes).
   (coaster property: #country)  *-<> (country property: #coasters).
   (coaster property: #creator)  *-<> (creator property: #coasters).
]
```

We find back all the information we put in our design, relationships, names, metaclasses...
This file can be directly imported in Moose and used to generate the metamodel code:

```st
(TonelParser parseString: ('/tmp/coaster.class.st' asFileReference contents )) do: #load. "Load the generated generator"
CoasterGenerator generate. "Generate the metamodel code"
```

<!-- ## Limitations -->


## And Next Time...

That's all folks for this post.
Next time, we will discuss about how to generate a dedicated metamodel for Famix instead of only Fame.
The exact same code generator will be used, but this time, we will have to deal with the desktop Ecode diagram tool for technical reasons.

Happy metamodeling!

<!-- ## Going Further -->
