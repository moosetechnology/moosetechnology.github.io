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

We saw that one can extend a meta-model by creating a new one based on the pre-existing entities.
It is also possible to connect two existing meta-models together.

To so one, one first need two meta-models to be connected.
As example, we will connect our coaster meta-model, with the world meta-model.
The world meta-model aims to represent the world, with its continent, countries, regions and cities.

### The world meta-model

We will not detail how to implement the world meta-model.
The figure below illustrated the meta-model.

![World meta-model](/img/posts/2021-05-15-connecting-meta-models/world-meta-model.drawio.svg){: .img-fill }

### Connecting world meta-model with Coaster meta-model

Our goal is to connect the coaster meta-model with the world meta-model.
To do so, we will connect the *country* concept of each meta-model.

![Connected meta-model](/img/posts/2021-05-15-connecting-meta-models/connected-meta-model.drawio.svg){: .img-fill }

As a first step, you should install both the coaster meta-model and the world meta-model.
Again, both are available in my [GitHub repository](https://github.com/badetitou/CoastersCollector).

Then, we create a new meta-model generator that will perform the connection.

```st
FamixMetamodelGenerator subclass: #ConnectMetamodelGenerator
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Connect-Model-Generator'
```

### Declare submetamodels

To connect together the two meta-models, we must first declare them in our connector meta-model.
To do so, we define the `#submetamodels` method.

```st
ConnectMetamodelGenerator class >> #submetamodels

    ^ { WorldMetamodelGenerator . CoasterCollectorMetamodelGenerator }
```

And, as for every meta-model generator, we define a prefix and a package name.

```st
ConnectMetamodelGenerator class >> #packageName

    ^ #'Connect-Model'
```

```st
ConnectMetamodelGenerator class >> #submetamodels

    ^ #'CM'
```

### Connect remote entities

Before creating the connection, we must declare, in the new meta-model, the entities that will be contected.
To do so, we declare them as remoteEntity.

```st
ConnectMetamodelGenerator >> #defineClasses
    super defineClasses.
    coasterCountry := self remoteEntity: #Country withPrefix: #CC.
    worldCountry := self remoteEntity: #Country withPrefix: #W
```

Then, it is possible to connect the two entities as classic one.

```st
ConnectMetamodelGenerator >> #defineRelations
    super defineRelations.
    coasterCountry - worldCountry
```

### Build a model with two connected submetamodels

Once the generator is created, we can generate the connection by generating the new meta-model.
To do so, execute in a playground:

```st
ConnectMetamodelGenerator generate
```

Then, it is possible to create a model with all the entities and to link the two meta-models.
In the following, we present a script that create a model.

```st
connectedModel := CMModel new.

coaster1 := CCCoaster new.
coaster2 := CCCoaster new.
coaster3 := CCCoaster new.

coasterFranceCountry := CCCountry new name: #'France'; yourself.
coasterFranceCountry addCoaster: coaster1.
coasterFranceCountry addCoaster: coaster2.

coasterGermanyCountry := CCCountry new name: #'Germany'; yourself.
coasterGermanyCountry addCoaster: coaster3.

wFranceCountry := WCountry new name: #'France'; yourself.
wGermanyCountry := WCountry new name: #'Germany'; yourself.

continent := WContinent new name: #Europe; yourself.
continent addCountry: wFranceCountry.
continent addCountry: wGermanyCountry.

coasterFranceCountry country: wFranceCountry.
coasterGermanyCountry country: wGermanyCountry.


connectedModel addAll: 
    { coaster1. coaster2 . coaster3 . 
    coasterFranceCountry . coasterGermanyCountry . 
    wFranceCountry . wGermanyCountry . continent }.
```

Based on the preceding model, it is possible to create query that will request the coaster and the world meta-model.
For instance, the following snippet count the number of coasters by country in the Europe continent:

```st
europe := (connectedModel allWithType: WContinent)
    detect: [ :continent | continent name = #Europe ].
(europe countries collect: [ :country | 
    country name -> country country coasters size ]) asDictionary 
```

## Conclusion

In this post, we saw how one can extend and connect meta-models using Famix Generator.
This feature is very helpfull when you need to improve a meta-model without modifying it directly.
If you need more control on the generated entities (*e.g.*, name of the relations, *etc.*), please have a look at the [create meta-model wiki page](/moose-wiki/Developers/CreateNewMetamodel).
