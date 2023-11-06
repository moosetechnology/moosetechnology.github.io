---
layout: post
title: "Model your Fame/Famix Meta-model using Graphical Editors"
subtitle: "Generate your Generator"
date: 2021-03-02 16:38:00 -0400
background: '/img/posts/bg-posts.jpg'
author: Vincent Aranega
comments: true
---

[Building metamodels](https://modularmoose.org/moose-wiki/Developers/CreateNewMetamodel) for Fame and Famix is currently done through a generator. 
The generator consists in various special methods that need to be implemented, _eg_, one that will create all metaclasses, another that will create all attributes for each metaclass,...
The metamodel's artifact are created programmatically using pure Smalltalk syntax extended with specific operators to easily express relationships and cardinalities (`<>-`, `*-*`, `<>-*`, ...).
For more details, please refer to the [article about "Coaster"]({% post_url 2021-02-04-coasters %}), written by Benoit Verhaeghe.

In this blogpost, I will show you how to use existing metamodeling diagraming tools to design your metamodel, and then to use the designed meta-model to generate your meta-model code for Fame in Moose.
In another blogpost, I will show you how to use this same generator to use graphical modeling tools to generate code for Famix.

## Why a Graphical Modeling Editor

The generator concept provided for Fame/Famix in Moose is practical, but can be intimidating in the first place.
Moreover, discovering an existing metamodel, or modifying an existing one can be hard as it requires to switch between various methods.
Some projects exist, as [PlantUMLPharoGizmo](https://github.com/fuhrmanator/PlantUMLPharoGizmo), to automatically build a graphical representation of an existing metamodel.
This approach considers the code of the meta-model as the main artifact and builds the diagram out of it.

In this post, we will consider the opposite: putting the diagram as the main artifact, then using the diagram as the basis for generating the code of the Fame/Famix generator.
Despite its possible complexity to describe a generator for a full huge metamodel, the syntax for building the generator is easy to deal with and easy to generate.
The general process is sketched in Figure 1.
From the `graphical modeling editor`, the produced meta-model is given to a
`dedicated code generator` that will produce the `Fame/Famix Generator`.

!["General Flow, from Graphical Model to Fame/Famix Generator Code"](/img/posts/2021-03-01-diagram-and-codegen/general-flow.png){: .img-fill }


## Reusing Existing Modeling Editor

Here is an interesting fact.
Building a graphical modeling editor from scratch is hard.
It can look easy in the first place, basically, it's only links and boxes, but it turns out that creating an editor that allows you to create a model, enforcings rules defined by a metamodel, is a complex task (without speaking about all graphical rules that need to be injected in the editor).
Consequently, we will reuse existing graphical modeling tools that already allow model metamodels.

Currently, there is no pure metamodeling editors for Pharo/Moose.
The only existing project is the [OpenPonk project](https://openponk.org/) which gives the ability to create UML models with XMI support, but UML proposes a lot of concepts that could lead to wrong design in the end (using wrong concepts).
On the other end, on the MDE community, [Ecore](https://www.eclipse.org/modeling/emf/) is an implementation of the [EMOF](https://www.omg.org/spec/MOF/2.4.1/PDF) and various desktop or web-based graphical editors exists.
Following then the flow we described earlier, the `Graphical Modeling Editor` will be one of the various graphical Ecore modeling editor.
The designed meta-model will be serialized in XMI (default output for those kind of editors), given to a code generator that will produce the Fame/Famix code generator.
The output of the code generator will follow the [tonel format](https://github.com/pharo-vcs/tonel) so it can be easily imported by Moose/Pharo.

### Aligning Ecore and Fame

As Fame, Ecore is a meta-meta-model that is used to create meta-models.
Various graphical tools exist for it, so we have a little bit of choice.
The entry point of our chain is an Ecore meta-model in a XMI format and the destination a Smalltalk class in a tonel format, but, the generated class will describe a generator for a Fame meta-model, not an Ecore one.
Consequently, we need to align Ecore concepts with Fame concepts.
This blogpost does not go into details about the full alignment between Ecore and Fame, but gives some keys that are used.

#### Metaclasses, EClass and Traits

Fame proposes two main concepts: `FM3Class` (metaclasses) and `FM3Traits` (traits).
On the contrary, Ecore only provides the concept of `EClass` to represent metaclasses, which is aligned with the metaclass concept of Fame.
Also, Ecore let users mark a metaclass as `interface`, which is a concept that comes close to the trait concept in Fame.
Thus, the alignment that we will keep here is:

* EClass -> FM3Class (metaclasses)
* EClass interface -> FM3Traits (traits)

#### Attributes

Attributes in Fame are modeled by the concept of `FM3Property`.
Attributes represent a property of a metaclass/trait and are usually bound to objects outside of the Fame "ecosystem" (as `String` that comes from Pharo for example).
The same concept can be found in Ecore with the `EAttribute` concept.
On the contrary of attributes in Fame, `EAttribute` in Ecore can own default values (and a set of other attributes that we will not consider here).
Consequently, we can directly align simple `EAttribute` to Fame attribute, ignoring the default value if one is set.
In this condition, the alignment is:

* EAttribute -> FM3Property

#### References

References in Fame are also modeled by the concept of `FM3Property`.
They can own a set of properties and can be marked as "containement", and they own a cardinality.
References represent relationships between metaclasses.
In Ecore, the `EReference` concept is the equivalent to `FM3Property`, that's then the one we use to model relationships.
The alignment is:

* EReferences -> FM3Property


#### Untackled Concepts from Ecore

Ecore proposes more concepts that are not directly aligned with Fame, _e.g._: `EEnumeration`, `EGenericType`,...
Thus, those concepts are not handled by the generator.


## Designing a Meta-model for Fame

Now that we know how we will represent Fame concepts with Ecore, we can use a graphical modeling tool to design Ecore meta-model to design Fame metamodels.

For this blogpost, three tools have been tested, two web-based one and a desktop one:

* [GenMyModel](https://www.genmymodel.com/) (web-based)
* [Ecore Diagram](https://www.eclipse.org/ecoretools/doc/) (desktop)
* [EMF.cloud](https://www.eclipse.org/emfcloud/) (web-based)

They all work well and have their pros and cons.
As example/illustration of this section I used GenMyModel (I worked there for some years, I know well the tool), but the design process is the same whatever the used tool.

When you design your meta-model for Fame using a graphical Ecore editor, you just focus on the concepts you want to represent.
The meta-model we are going to design is the one presented by Benoit in his [article about "Coaster"]({% post_url 2021-02-04-coasters %}).

The designed Ecore meta-model can be found [at this address](https://app.genmymodel.com/api/repository/aranega/Coaster) and it looks like this:

!["Coaster Meta-model in Ecore"](https://app.genmymodel.com/api/projects/_vgiYcHuVEeuCM8KqVoRWiA/diagrams/_vgiYcnuVEeuCM8KqVoRWiA/svg){: .img-fill }

In addition, we also set two specific properties of the base `EPackage`: `nsURI` and `nsPrefix`.
While the `nsURI` is more internal/maintenance related (it can be used to propose a unique identifier towards this version of the metamodel), `nsPrefix` is really important as it will set the prefix that will be used later for the generation of the generator.
I thus set the two values to:

* `nsURI` = `https://coaster/1.0`
* `nsPrefix` = `Ct`

With this, we are ready to generate the code of our generator.

## Generate your Generator

The code generator is coded in Python using the PyEcore library to be able to decode/resolve Ecore files.
The generator goes across the full meta-model at least once to perform model validation, tries to correct some issues (like spaces at the beginning or end or names), and raises an error if it cannot generate the code for some reasons.
Once the meta-model is validated, it generates the Fame generator code using [Mako](https://www.makotemplates.org/), a template engine.


### Install the code generator

The generator installation is pretty simple if you have the right tools.
You can find the code on GitHub at [this address](https://github.com/aranega/famegenerator).
You can install all the dependencies manually on your system, but the proper way of dealing with the dependencies would be to use a virtual environment (a virtualenv).
Virtualenvs creation and activation can be somehow complicated if you are not used to it.
To ease this step, the repository contains a `Pipfile` to be used with `pipenv`.
If you don't have `pipenv` installed on your machine, feel free to install it, it helps you creating virtualenvs from `Pipfile` with all the required dependencies and ease also their activation.
In our case, with `pipenv`, the installation is pretty forward:

```bash
$ git clone https://github.com/aranega/famegenerator.git
$ cd famegenerator
$ pipenv install
```

And you're good to go, `pipenv` creates a virtualenv and installs all the dependencies isolated in your virtualenv.
Now, each time you want to use the created virtualenv, you can just enter in the directory where the code of the generator is installed and just activate the virtualenv:

```bash
$ pipenv shell
```


### Generate the code of your Generator

If you did your meta-model online, there are two options for you.
Either you can download the `.ecore` on your machine and generate the code from there, or you can give to the generator a valid URL where the XMI of your ecore meta-model is located.
Whatever option you'll choose, you can generate the code of your generator this way:

```bash
$ python famegen.py <URL_OR_FILE> -o mymeta.class.st
```

This will generate the code of your generator in a tonel format in the file `mymeta.class.st`.
If you don't use the option `-o`, the generated code is displayed on standard output.

Here is the line used to generate the generator of the `Coaster` meta-model:

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

We find back all the information we put in our design, relationships, names, metaclasses, ...
This file can be directly imported in Moose and used to generate the meta-model code:

```st
(TonelParser parseString: ('/tmp/coaster.class.st' asFileReference contents )) do: #load. "Load the generated generator"
CoasterGenerator generate. "Generate the meta-model code"
```

<!-- ## Limitations -->

## And Next Time...

That's all folks for this post.
Next time, we will discuss about how to generate a dedicated meta-model for Famix instead of only Fame.
The exact same code generator will be used, but this time, we will have to deal with the desktop Ecore diagram tool for technical reasons.

Happy metamodeling!

<!-- ## Going Further -->
