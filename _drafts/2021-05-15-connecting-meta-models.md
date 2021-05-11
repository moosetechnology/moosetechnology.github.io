---
layout: post
title: "Connecting/Extending meta-models"
date: 2021-05-15 12:00:00 -0400
background: '/img/posts/2021-05-15-connecting-meta-models/bg-post.png'
author: Benoît Verhaeghe
comments: true
---

Sometimes, a model does not have all the information you want.
Or, you want to connect it with another one.
A classic example is going from an abstract level to another.

In this blog post, I will show you how to extend and connect a meta-model with another.
We will use the [Coaster example]({% post_url 2021-02-04-Coasters %}).

## Extending the Coaster meta-model

The Coaster meta-model is super great (I know... it is mine :smile: ).
Using it, one can manage its collection of coasters.

However, did you notice that there is only one kind of Creator possible: the brewery.
It is not great because some coasters are not created by breweries but for events.
My model is not able to represent this situation.
So, there are two possibilities: I can fix my meta-model, or I can extend it with a new concept.
Here, we will see how I can extend it.

![Extended Coaster meta-model](/img/posts/2021-05-15-connecting-meta-models/extended-coaster-model.drawio.svg){: .img-fill }

As presented in the above figure, we add the Events concept as a kind of Creator.

### Configuration

As a first step, we need the original Coaster meta-model generator loaded in our image.
We can download it from my [Coaster GitHub repository](https://github.com/badetitou/CoastersCollector).

You should have a named `CoasterCollectorMetamodelGenerator` in your image.
This is the generator of the original meta-model.
We will now create another generator connected with the original one.

First, we create a new generator for our extended meta-model.

```st
FamixMetamodelGenerator subclass: #CoasterExtendedMetamodelGenerator
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'CoasterCollector-ExtentedModel-Generator'
```

Then, we link our generator with the original one.
To do so, we will use the [submetamodels feature](/moose-wiki/Developers/CreateNewMetamodel#introducing-submetamodels) of the generator.
We only have to implement the `#submetamodels` method in the class side of our new generator.
This method should return an array including the generators of the submetamodels.

```st
CoasterExtendedMetamodelGenerator class >> #submetamodels
    ^ { CoasterCollectorMetamodelGenerator }
```

Finally, as for a classic meta-model generator, we define a package name and a prefix.

```st
CoasterExtendedMetamodelGenerator class >> #packageName
    ^ #'CoasterExtended-Model'
```

```st
CoasterExtendedMetamodelGenerator class >> #prefix
    ^ #'CCE'
```

### Define entities

Creating new concepts in the new meta-model is done following the same approach as for classic meta-model generator.
In our example, we add the `Event` class.
Thus, we create the method `#defineClasses` with the new entity.

```st
CoasterExtendedMetamodelGenerator >> #defineClasses
    super defineClasses.
    event := builder newClassNamed: #Event.
```

### Declare entity of submetamodels

To extend the original meta-model, we first need to identify the entities of the original meta-model we will extend.
In our case, we only extend the `Creator` entity.
Thus, we declare it in the `#defineClasses` method.
To do so, we use the method `#remoteEntity:withPrefix:`.
The prefix is used to allow multiple entities coming from different submetamodels but with the same name.

```st
CoasterExtendedMetamodelGenerator >> #defineClasses
    super defineClasses.
    event := builder newClassNamed: #Event.

    "Remote entities"
    creator := self remoteEntity: #Creator withPrefix: #CC 
```

> We refer a remote entity by sending `#remoteEntity:withPrefix:` to `self` and not using the `builder`.
> Indeed, the entity is already created.

### Using remote entity

Once the declaration done, one can use the remote entities as classic entities in the new generator.
In our example, we will create the hierarchy between `Creator` and `Event`.

```st
CoasterExtendedMetamodelGenerator >> #defineHierarchy
    super defineHierarchy.
    event --|> creator
```

### Generate

Once everything is defined, as for classic generator, we generate the meta-model.
To do so, execute in a playground:

```st
CoasterExtendedMetamodelGenerator generate
```

The generation creates a new package with the `Event` entity.
It also generates a class named `CCEModel` used to create an instance of our extended meta-model.

### Use the new model

It is now possible to use the new meta-model with the `Event` concept.
For instance, one can perform the following script in a playground to create a little model.

```st
myExtendedModel := CCEModel new.
myExtendedModel add: (CCBrewery new name: 'Badetitou'; yourself).
myExtendedModel add: (CCEEvent new name: 'Beer party'; yourself)
```

## Connecting meta-models

## And for Famix
