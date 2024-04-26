---
layout: page
author: Benoit Verhaeghe
background: '/img/bg-wiki.jpg'
title: 'Famix Java'
toc: true
---

> This is a work in progress page that tries to help beginners. Do not hesitate to contact us for more information

Famix Java is the metamodel included in Moose used to represent any Java application.
As you can imagine, the metamodel is rather than complicated.
For instance, Famix Java uses a lot the [traits concept](/Developers/predefinedEntities).

We will present here some incorrect, but simplify and useful view to enable beginners to use Famix Java.

## Class neighborhood

```mermaid!
classDiagram
    class FamixJavaPackage {

    }


    class FamixJavaClass {

    }

    class FamixJavaInheritance {

    }

    class FamixJavaMethod {
        
    }

    class FamixJavaAttribute {
        
    }

    FamixJavaPackage *--> FamixJavaClass : types
    FamixJavaPackage --> FamixJavaPackage : parentPackage

    FamixJavaClass *--> "*" FamixJavaMethod : methods
    FamixJavaClass *--> "*" FamixJavaAttribute : attributes

    FamixJavaInheritance <-- "*" FamixJavaClass : superInheritances
    FamixJavaClass --> "*" FamixJavaInheritance : subInheritances
```

## Type neighborhood

```mermaid!
classDiagram
    FamixJavaMethod "*" <--* FamixJavaType : methods
    FamixJavaAttribute "*" <--*  FamixJavaClass: attributes
    FamixJavaAttribute "*" <--*  FamixJavaInterface: attributes

    FamixJavaType <|--  FamixJavaClass
    FamixJavaType <|--  FamixJavaEnum
    FamixJavaType <|--  FamixJavaInterface
    FamixJavaType <|--  FamixJavaPrimitiveType
    FamixJavaType <|--  FamixJavaParameterType
    FamixJavaType <|--  FamixJavaWildcard
    FamixJavaClass <|-- FamixJavaException
    FamixJavaClass <|-- FamixJavaParametricClass
    FamixJavaInterface <|-- FamixJavaParametricInterface
```

### Focus on Parametric Type

Parametric types are probably the most hard to understand.
We made a [full blog post about this subject]({% post_url 2023-07-13-parametric %}).

![Parametric schema](./img/parametrics.drawio.png){: .img-fluid }
