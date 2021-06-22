---
layout: post
title: "Towards analyzing TypeScript with Moose"
subtitle: "I discuss an imperfect but useful approach to analyze TypeScript projects using existing elements."
date: 2021-06-23 12:00:00 -0400
background: '/img/posts/2021-06-04-plantUML-for-metamodel/bg-post.jpg'
author: Christopher Fuhrman
comments: true

---

TypeScript is a more and more popular programming language, and so it would be great if we could analyze TypeScript projects using Moose.
As of the time of writing, no meta-model (or importer) exists for the TypeScript language in Moose.
First, what are the pieces of the puzzle needed analyze TypeScript with Moose? 
Before we consider TypeScript, let's look at how things work with Java:

![Elements of analyzing a Java project](/img/posts/towards-analyzing-typescript/puzzle.drawio.svg){: .img-fill }

[VerveineJ](https://github.com/moosetechnology/VerveineJ) is the *importer* that can generate *models* of Java files, allowing us to do analyses in Pharo/Moose.

## What's needed to analyze TypeScript?

If we want to do the same thing for TypeScript, we'd need:

- an equivalent of VerveineJ (importer) for TypeScript files,
- a Famix model of TypeScript.
 
Creating a parser and importer for TypeScript is no small task, but TypeScript is a popular environment and we can use [`ts-morph`](https://ts-morph.com/) to faciliate the navigation of the TypeScript AST.
There's also a very cool [visualization of TypeScript ASTs](https://ts-ast-viewer.com/#), which will be useful for understanding and debugging.

Designing a new meta-model for TypeScript is definitely not trivial, because it requires a deep understanding of the language.
On the other hand, once a meta-model exists, it's easy to *generate* using FamixNG domain-specific language.

Pragmatically speaking, do we need a perfect model of TypeScript to analyze it?

## abap2famix

> "All models are wrong, but some are useful." --[maybe not](https://en.wikipedia.org/wiki/All_models_are_wrong) George Box...

By searching the web for TypeScript and Moose I discovered a GitHub project called [pascalerni/abap2famix](https://github.com/pascalerni/abap2famix). 
It is an [ABAP](https://en.wikipedia.org/wiki/ABAP) importer (written in TypeScript) that models ABAP projects using [FAMIX 3.0 (compatibility meta-model for Java)](https://www.researchgate.net/publication/265428652_MSE_and_FAMIX_30_an_Interexchange_Format_and_Source_Code_Model_Family).
Java and ABAP are indeed different languages, but perhaps the differences are not so important if we want to do some static analysis?
Seems like a pragmatic approach!

Looking at the node packages used by abap2famix I discovered [famix](https://www.npmjs.com/package/famix) (a TypeScript implementation of Famix, which facilitates creating FAMIX 3.0 entities from TypeScript).
Its source is at [pascalerni/famix](https://github.com/pascalerni/famix), and I could see that much of it was generated, e.g., in [class.ts](https://github.com/pascalerni/famix/blob/d68d11cbbc3f8423dcd1acd46da602ea13e2b1f4/src/model/famix/class.ts#L1) there's proof it was not written by hand:

```typescript
// automatically generated code, please do not change

import {FamixMseExporter} from "../../famix_mse_exporter";
import {Type} from "./../famix/type";

export class Class extends Type {
  ...
}
```

How was this code generated? The answer is the fork of FameJava at [pascalerni/FameJava](https://github.com/pascalerni/FameJava), namely the [Famix30Codegen.java](https://github.com/pascalerni/FameJava/blob/master/test/ch/akuhn/fame/codegen/target/Famix30Codegen.java) file.
The original FameJava was used to generate the Java API for use with FAMIX 3.0 metamodel.
This fork generates (via Java) a TypeScript API.
Clever and useful!

So, let's try 

To integrate:

- Rapid analysis of TypeScript using a Java Metamodel
- explain what Pascal Erni did for ABAP
- Propose reusing his FAME API in TypeScript combined with ts-morph (TS parser)
- explain a couple limitations, point to the results (repos) from the students
- talk about challenges of defining a TS metamodel

## Conclusion


