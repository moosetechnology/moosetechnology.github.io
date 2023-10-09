---
author: Benoit Verhaeghe
layout: post
title:  "Test your Moose code using CIs"
date: 2022-10-28 00:00:00 +200
tags: CI
background: '/img/posts/bg-posts.jpg'
comment: true
---

You have to test your code!

I mean, *really*.

But sometimes, testing is hard, because you do not know how to start (often because it was hard to start with TDD or better XtremTDD :smile:).

One challenging situation is the creation of mocks to _represent_ real cases and use them as test resources.
This situation is common when dealing with code modeling and meta-modeling.

Writing a model manually to test features on it is hard.
Today, I'll show you how to use GitHub Actions as well as GitLab CI to create tests for the Moose platform based on real resources.

---

First of all, let's describe a simple process when working on modeling and meta-modeling.

```mermaid!
flowchart LR
    SourceCode(Source Code) --> Parse --> modelfile(Model File) --> Import --> model(Model in Memory) --> Use
```

When analyzing a software system using MDE, everything starts with parsing the source code of the application to produce a model.
This model can then be stored in a file.
Then, we import the file into our analysis environment, and we use the concrete model.

All these steps are performed before using the model.
*However*, when we create tests for the `Use` step, we do not perform all the steps before.
We likely just create a mock model.
Even if this situation is acceptable<!-- comfortable -->, it is troublesome <!-- why? --> because it disconnects the test from the tools (which can have bugs) that create the model. <!-- makes easy un-synchronization between tools to manipulate a model, and tools that create a model. -->
<!-- Even if this situation seems  -->

One solution is thus not to create a mock model, but to create mock source code files.

## Proposed approach

Using mock source code files, we can reproduce the process for each test (or better, a group of tests :wink:)

```mermaid!
flowchart LR
    SourceCode(Mock Source Code) --> Parse(Parse with Docker) --> modelfile(Model File) --> Import(Import with script) --> model(Model in Memory) --> Test
```

In the following, I describe the implementation and set-up of the approach for analyzing Java code, using Pharo with Moose.
It consists of the following steps:

- Create mock resources
- Create a bridge from your Pharo image to your resources using PharoBridge
- Create a GitLab CI or a GitHub Action
- Test :heart:

## Create mock resources

The first step is to create mock resources.
To do so, the easiest way is to include them in your git repository.

You should have the following:

```raw
> ci // Code executed by the CI
> src // Source code files
> tests // Tests ressources
```

Inside the `tests` folder, it is possible to add several subfolders for different test resources.

## Create a Pharo Bridge

To easily use the folder of the test resource repository from Pharo, we will use the [GitBridge project](https://github.com/jecisc/GitBridge).

The project can be added to your Pharo Baseline with the following code fragment:

```st
spec
    baseline: 'GitBridge'
    with: [ spec repository: 'github://jecisc/GitBridge:v1.x.x/src' ].
```

Then, to connect our Pharo project to the test resources, we create a class in one of our packages, a subclass of `GitBridge``.

A full example would be as follows:

```st
Class {
    #name : #MyBridge,
    #superclass : #GitBridge,
    #category : #'MyPackage-Bridge'
}

{ #category : #initialization }
MyBridge class >> initialize [

    SessionManager default registerSystemClassNamed: self name
]

{ #category : #'accessing' }
MyBridge class >> testsResources [
    ^ self root / 'tests'
]
```

The method `testsResources` can then be used to access the local folder with the test resources.

**Warning**: this setup only works locally.
To use it with GitHub and GitLab, we first have to set up our CI files.

## Set up CI files

To set up our CI files, we first create in the `ci` folder of our repository a `pretesting.st` file that will execute Pharo code.

```st
(IceRepositoryCreator new
    location: '.' asFileReference;
    subdirectory: 'src';
    createRepository) register
```

This code will be run by the CI and register the Pharo project inside the Iceberg tool of Pharo.
This registration is then used by GitBridge to retrieve the location of the test resources folder.

Then, we have to update the `.smalltalk.ston` file (used by every Smalltalk CI process) and add a reference to our `pretesting.st` file.

```st
SmalltalkCISpec {
  #preTesting : SCICustomScript {
    #path : 'ci/pretesting.st'
  }
  ...
}
```

### Set up GitLab CI

The last step for GitLab is the creation of the `.gitlab-ci.yml` file.

This CI can include several steps.
We now present the steps dedicated to testing the Java model, but the same steps apply to other programming languages.

First, we have to parse the tests-resources using the [docker version of VerveineJ](https://hub.docker.com/r/badetitou/verveinej)

```yml
stages:
  - parse
  - tests

parse:
  stage: parse
  image:     
    name: badetitou/verveinej:v3.0.0
    entrypoint: [""]
  needs:
    - job: install
      artifacts: true
  script:
    - /VerveineJ-3.0.0/verveinej.sh -Xmx8g -Xms8g -- -format json -o output.json -alllocals -anchor assoc -autocp ./tests/lib ./tests/src 
  artifacts:
    paths:
      - output.json
```

The `parse` stage uses the `v3` of VerveineJ, parses the code, and produces an `output.json` file including the produced model.

Then, we add the common `tests` stage of Smalltalk ci.

```yml
tests:
  stage: tests
  image: hpiswa/smalltalkci
  needs:
    - job: parse
      artifacts: true
  script:
    - smalltalkci -s "Moose64-10"
```

This stage creates a new `Moose64-10` image and performs the CI based on the `.smalltalk.ston` configuration file.

### Setup GitHub CI

The last step for GitLab is the creation of the `.github/workflows/test.yml` file.

In addition to a common smalltalk-ci workflow, we have to configure differently the checkout step, and add a step that parses the code.

For the checkout step, GitBridge (and more specifically Iceberg) needs the history of commits.
Thus, we need to configure the checkout actions to fetch the all history.

```yml
- uses: actions/checkout@v3
  with:
    fetch-depth: '0'
```

Then, we can add a step that runs VerveineJ using its docker version.

```yml
- uses: addnab/docker-run-action@v3
  with:
    registry: hub.docker.io
    image:  badetitou/verveinej:v3.0.0
    options: -v ${{ github.workspace }}:/src
    run: |
      cd tests
      /VerveineJ-3.0.0/verveinej.sh  -format json -o output.json -alllocals -anchor assoc .
      cd ..
```

Note that before running VerveineJ, we change the working directory to the tests folder to better deal with source anchors of Moose.

You can find a [full example in the FamixJavaModelUpdater repository](https://github.com/badetitou/FamixJavaModelUpdater/blob/main/.github/workflows/test.yml)

## Test

The last step is to adapt your tests to use the model produced from the mock source.
To do so, it is possible to remove the creation of the mock model by loading the model.

Here's an example:

```st
externalFamixClass := FamixJavaClass new
  name: 'ExternalFamixJavaClass';
  yourself.
externalFamixMethod := FamixJavaMethod new
  name: 'externalFamixJavaMethod';
  yourself.
externalFamixClass addMethod: externalFamixMethod.
myClass := FamixJavaClass new
  name: 'MyClass';
  yourself.
externalFamixMethod declaredType: myClass.
famixModel addAll: { 
  externalFamixClass.
  externalFamixMethod.
  myClass }.
```

The above can be converted into the following:

```st
FJMUBridge testsResources / 'output.json' readStreamDo: [ :stream | 
    famixModel importFromJSONStream: stream ].
famixModel rootFolder: FJMUBridge testsResources pathString.

externalFamixClass := famixModel allModelClasses detect: [ :c | c name = 'ExternalFamixJavaClass' ].
myClass := famixModel allModelClasses detect: [ :c | c name = 'MyClass' ].
externalFamixMethod := famixModel allModelMethods detect: [ :c | c name = 'externalFamixJavaMethod' ].
```

## Congrats

You can now test your code on a model generated as a real-world model!

It is clear that this solution slows down tests performance, however. But it ensures that your mock model is well created, because it is created by the parser tool (importer).

A good test practice is thus a mix of both solutions, classic tests in the analysis code, and full scenario tests based on real resources.

Have fun testing your code now!

> Thanks [C. Fuhrman](https://github.com/fuhrmanator) for the typos fixes. :banana:
