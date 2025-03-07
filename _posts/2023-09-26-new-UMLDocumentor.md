---
layout: post
title: "Generate a class diagram visualization for a meta-model"
date: 2023-09-26 12:00:00 -0400
background: '/img/posts/2023-09-26-new-UMLDocumentor/bg-post.jpg'
author: Nicolas Anquetil
comments: true
tags: Famix-tools
---

*This post is the first in a serie dedicated to* Famix Tools

When creating or studying a meta-model, it is often convenient to be able to "see" it as a whole.

UML looks like a natural solution for this.

So in the past we had a tool to create UML diagrams of the meta-models through PlantUML (a small language and a tool to generate UML diagrams).
The post [Generate a plantUML visualization for a meta-model]({% post_url 2021-06-04-plantUML-for-metamodel %}) explained how to use this tool

But the tool had some limitations, one of which was that it was not easy to add a different backend than PlantUML.

Therefore, inspired by the previous tool, we redesigned a new one, **FamixUMLDocumentor**, with a simpler API and the possibility to add new backends.

## Simple Use

We illustrate the use with the same [Coaster example]({% post_url 2021-02-04-coasters %}) already used previously.
You can also experiment with `FDModel`, a small meta-model used for testing.

You can create a PlantUML script for a UML class of your metamodel with:

```smalltalk
FamixUMLDocumentor new
  model: CCModel ;
  generate ;
  exportWith: (FamixUMLPlantUMLBackend new).
```

The result will be a PlantUML script that you can paste into `https://plantuml.org/` to get this UML class diagram:

![Generated UML class of the Coaster meta-model](/img/posts/2023-09-26-new-UMLDocumentor/CCModel-plantUML.png){: .img-fluid}

## FamixDocumentor API

The API for the documenter is as follow:

- `model:` -- adds a meta-model to export. Several meta-models can be exported jointly by adding them one after the other.
By default each meta-model is automatically assigned a color in which its entities will be drawn.
- `model:color:` -- same as previous but manually assign a `Color` to the meta-model.
- `onlyClasses:` -- specifies a list of classes to export. It can replace the use of `model:`.
- `excludeClasses:` -- specifies a list of classes to exclude from the export.
Typically used with `model:` to remove from the UML some of the meta-model's classes.
Can also be used to exlude "stub" classes (see `beWithStubs`).
- `beWithStubs` -- Indicates to also export the super-classes and used traits of exported classes, even if these super-classes/traits or not part of the meta-models. These stubs have an automatically selected color different from the meta-models displayed.
- `beWithoutStubs` -- opposite of the preceding. This is the default option.
- `generate` -- creates an internal representation of a UML class diagram according to the configuration created with the preceding messages.
- `exportWith:` -- exports the internal representation with the "backend" given (for example: `FamixUMLPlantUMLBackend` in the example above)

## FamixUML Backends

The backend is normally called by the `FamixUMLDocumentor` but can be called manually.
For example, the image above can be exported in a PlantUML script with:

```smalltalk
documentor := FamixUMLDocumentor new.
documentor
    model: CCModel ;
    generate.
FamixUMLPlantUMLBackend new export: documentor umlEntities.
```

 (Compare with the example given above)

Backends have only one mandatory method:
- `export:` -- Exports the collection of umlEntities (internal representation) in the format specific to the backend.

New backends can be created by subclassing `FamixUMLAbstractBackend`.

There is a `FamixUMLRoassalBackend` to export the UML diagram in Roassal (visible inside Pharo itself), and a  `FamixUMLMermaidBackend` to export in Mermaid format (similar to PlantUML).

There is a `FamixUMLTextBackend` that outputs the UML class diagram in a textual form. 
By default it returns a string but this can be changed:
- `toFile:` -- Instead of putting the result in a string, will write it to the file whose name is given in argument.
- `outputStream:` -- specifies a stream on which to write the result of the backend.

`FamixUMLPlantUMLBackend` and `FamixUMLMermaidBackend` are subclasses of this `FamixUMLTextBackend` (therefore they can also export to a file).
