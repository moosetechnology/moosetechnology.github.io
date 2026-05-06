---
authors:
- ClotildeToullec
title: "Introducing Java initializers"
subtitle: The Famix metamodel evolves again
date: 2026-02-17
tags:
- meta-model
---

In Java, we can define behavior that is executed exclusively at the initialization of an instance. For now, our metamodel represented these behaviors as methods. This evolution represents them as `Initializers`.

## What are initializers?

We consider as initializers the following elements:

- Constructors: they are called when creating a new instance. When a constructor is called, if no explicit call is defined, it implicitly calls the default no-argument constructor, that calls the no-argument constructor in the superclass. We do not represent implicit constructors and these invocations.
- Initialization blocks: blocks that are executed when a new instance is created. They are copied by the Java compiler into each constructor and avoid code duplication. We do not represent this implicit invocation.
- In Famix: the `<Initializer>` method: we create a method to hold all attribute definitions in a type.

## Why do we change?

The main motivation for this change is to adapt the metamodel to the needs of building call graphs.
Call graphs must be able to create the implicit invocations described above and to distinguish between the 3 types of initializers.

Another motivation is to differentiate between initializers and actual methods.
In analyses, we often need to focus on methods and initializers can add noise when treated as actual methods, especially the `<Initializer>` method.

## What's changing

We introduce `FamixJavaInitializer`, a subclass of `FamixJavaMethod`.
An `Initiliazer` has 2 properties:

- `#isInitializationBlock`: boolean, `false` by default.
- `#isConstructor`: boolean, derived. In java, a constructor is an initializer with the same name as its parent type, with no declared type (or `void` as declared type).

We do not merge all initializers as we did before, but we still merge similar initializers, that will always be called together.
In a Java model, a type (`TWithMethods`) can define a maximum of 4 initializers (besides constructors):

- An instance initialization block, that is the merge of all instance initialization blocks.
- An instance-side `<Initializer>` method, similar to the one we created before.
- A static initialization block, merge of all static initialization blocks (`isClassSide`is true).
- A static `<Initializer>`method (`isClassSide`is true) for static attributes definition.

When inspecting a Java model, initializers can now be found under `Initializers` and `Model initializers`.
