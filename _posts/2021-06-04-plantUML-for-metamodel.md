---
layout: post
title: "Generate a plantUML visualization for a meta-model"
date: 2021-06-04 12:00:00 -0400
background: '/img/posts/2021-06-04-plantUML-for-metamodel/bg-post.jpg'
author: Théo Lanord
comments: true
tags: documentor
---

## Note:

This post describes a tool that has been replaced by a new `FamixUMLDocumentor`.
The new tool is described [in another post]({% post_url 2023-09-26-new-UMLDocumentor %}).

## Old post content

When you are interested in a meta-model of which you are not the creator, it is sometimes difficult to understand it only using the declarations in the code.
It would be best if you could actually visualize it in a different way.
What better way to go back to a very efficient meta-model visualization tool: UML.

In this blog, I will show you how to generate [plantUML](https://plantuml.com/) code from a generated meta-model.
For that, I will take the example of the evolution of the meta-model on coasters:

- [Coasters collection]({% post_url 2021-02-04-Coasters %})
- [Connecting/Extending meta-models]({% post_url 2021-05-15-connecting-meta-models %})

There is no need to do these posts to understand this one.
I would even say that this is precisely the subject: to study an unknown meta-model.

## Prerequisite and details

First of all, and if it has not already been done, do not forget to download and generate the meta-models using its generator.
For example, for the basic [Coasters collection]({% post_url 2021-02-04-Coasters %}), the code is available on [Coaster GitHub repository](https://github.com/badetitou/CoastersCollector) and it can be generate with:

```st
CoasterCollectorMetamodelGenerator generate
```

`FamixMMUMLDocumentor`, the tool I am going to present to you, is based on the generated meta-model. Therefore, it is particularly suitable for models with subMetamodels (*Cf.* beWithStub option).
It is important to note that another tool, based on the meta-model builder, exists : `FmxMBPlantTextVisitor`. It can be interesting if you need to display the compositions.

I would also like to make one last remark, most of the information given in this post can be found in the comment of the `FamixMMUMLDocumentor` class.
Finally, there is the [plantUML server](http://www.plantuml.com/plantuml/uml/SyfFKj2rKt3CoKnELR1Io4ZDoSa70000) to run your plantUML code directly on the web.
So let's continue and generate our visualizations! :smile:

## Global approach

### Generation on the whole meta-model

Let's say we know that there is a meta-model on coasters whose builder is `CoasterCollectorMetamodelGenerator`.
Since we need the generated model and not the builder, we will look at the prefix defined in `CoasterCollectorMetamodelGenerator class >> #prefix` and deduce the model name, which consists of the model prefix followed by the word `Model`.

In this case, for `CoasterCollectorMetamodelGenerator`, the model is called `CCModel`.
From here, we have all the elements to generate the plantUML code associated with the model via the following code:

```st
FamixMMUMLDocumentor new
 model: CCModel ;
 generatePlantUMLModel.
```

The generation is done by instantiating a `FamixMMUMLDocumentor` for which we provide the model (*model:*) and ask for the complete generation for this last one (*generatePlantUMLModel*).

![UML representation of Coaster meta-model](/img/posts/2021-06-04-plantUML-for-metamodel/CCModel-plantUML.svg){: .img-fill }

We can now compare the generated UML representation to the basic one that helped create the generator or that has been used to generate the generator :smile: (*Cf.* [Model your Fame/Famix meta-model using Graphical Editors]({% post_url 2021-03-01-diagram-and-codegen %})).

!["coasters UML"](/img/posts/2021-02-04-Coasters/coaster-model.drawio.svg){: .img-fill }

We can observe a UML diagram that is almost identical.
Only `CCModel` is additional.
However, generation options allow solving this problem (and many others).

## Generation options

### generatePlantUMLModelWithout

Indeed, it is possible to ask to generate the plantUML code without a defined collection of entities. For example, if you do not want the `CCModel` to appear.

```st
FamixMMUMLDocumentor new
 model: CCModel ;
 generatePlantUMLModelWithout: { CCModel }.
```

![UML representation (option Without) of Coaster meta-model](/img/posts/2021-06-04-plantUML-for-metamodel/CCModel-plantUML-Without.svg){: .img-fill }

It is important to note that it is necessary to give the entities themselves and not their names.
That is to say that it is necessary to add their prefix.
For example, the entity associated with the name `Coaster` is `CCCoaster`.

### generatePlantUMLWith

It is also possible to do the opposite.
That is to say to select only the entities to generate.

```st
FamixMMUMLDocumentor new
 model: CCModel ;
 generatePlantUMLWith: { CCCoaster . CCCreator . CCBrewery }.
```

![UML representation (option With) of Coaster meta-model](/img/posts/2021-06-04-plantUML-for-metamodel/CCModel-plantUML-With.svg){: .img-fill }

This can be useful if you are interested in certain entities.

### beWithStub

Finally, there is one last exciting possibility.
If we take the case of the evolution of the coasters meta-model extended in terms of creators [Connecting/Extending meta-models]({% post_url 2021-05-15-connecting-meta-models %}).

![Extended Coaster meta-model](/img/posts/2021-05-15-connecting-meta-models/extended-coaster-model.drawio.svg){: .img-fill }

Let's generate the plantUML code on the meta-model and observe.

```st
FamixMMUMLDocumentor new
 model: CCEModel ;
 generatePlantUMLModelWithout: { CCEModel }.
```

![UML representation of Extended Coaster meta-model](/img/posts/2021-06-04-plantUML-for-metamodel/CMModel-plantUML-Without.svg){: .img-fill }

We can say that the representation is deceiving, but it is only a representation of what is declared in the meta-model.
However, the meta-model has a subMetamodel, so we have to look for these dependencies in it.
For this, there is the `beWithStub` option.

```st
FamixMMUMLDocumentor new
 beWithStub;
 model: CCEModel ;
 generatePlantUMLModelWithout: { CCEModel . MooseModel }.
```

![UML representation with stub of Extended Coaster meta-model](/img/posts/2021-06-04-plantUML-for-metamodel/CMModel-plantUML-Without-beWithStub.svg){: .img-fill }

We can see that `Event` inherits from an external class `Creator`, coming from the subMetamodel `CoasterCollectorMetamodelGenerator`.

It would indeed be interesting to generate the subMetamodel view as well in order to have a better overall view, maybe an improvement track?

### Output to a file

Each option is available in text or file output via the following methods:

- `generatePlantUMLModel` / `generatePlantUMLModelFile:`
- `generatePlantUMLModelWithout:` / `generatePlantUMLModelFile:without:`
- `generatePlantUMLWith:` / `generatePlantUMLFile:with:`

## Application to a larger model

To finalize this post, we will generate a larger meta-model that aggregates all the notations available in the tool.
To do this, I chose `FASTModel`, a method syntax analysis meta-model, available with this [moosetechnology GitHub repository](https://github.com/moosetechnology/FAST).

```st
FamixMMUMLDocumentor new
 beWithStub;
 model: FASTModel;
 generatePlantUMLModelWithout: { FASTModel . MooseModel }.
```

![UML representation with stub of FASTCore](/img/posts/2021-06-04-plantUML-for-metamodel/FASTCore-plantUML-Without-beWithStub.svg){: .img-fill }

### Notations

In summary, we have 5 specific notations:

- Internal entity notations:
  - Class:    Black C on white background
  - Trait:    Black T on grey background
- External entity notations:
  - Class:    Black C on yellow background with `External` label
  - Trait:    Black T on yellow background with `External` label
- Use of traits:    Dashed arrow

The rest of the notations follows the UML standard.

## Conclusion

In this post, we have seen how to visualize a meta-model using `FamixMMUMLDocumentor`.
This feature is handy for understanding complex meta-models and allows (almost) automatic documentation.
