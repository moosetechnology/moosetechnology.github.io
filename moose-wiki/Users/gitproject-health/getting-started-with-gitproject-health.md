---
layout: page
author: Benoit Verhaeghe
background: '/img/bg-wiki.jpg'
title: 'Getting started with GitProject Health'
toc: true
---

This page presents the starting point if you want to have high-level information about your repositories' _health_.

## What is GitProject Health

GitProject health is a suit of importers, models, and visualizations, that enable the analysis of your repositories.
For instance, you will be able to see the repositories with failing pipelines, the numbers of commits by projects, the authors of commits, _etc_.
It is also possible to link the repository commits with a Famix model to have even more information.

## Installation GitProjectHealth

To install GitProjectHealth, you'll need a Moose11+ image (see the [install Moose page](/moose-wiki/Beginners/InstallMoose)).

In the Moose image, in a playground (`Ctrl+O`, `Ctrl+W`), perform:

```st
Metacello new
  repository: 'github://moosetechnology/GitProjectHealth:main/src';
  baseline: 'GitLabHealth';
  onConflict: [ :ex | ex useIncoming ];
  onUpgrade: [ :ex | ex useIncoming ];
  onDowngrade: [ :ex | ex useLoaded ];
  load
```

This command will load the GitProjectHealth project as well as all its Pharo dependencies.

> The project was originally made for GitLab repositories analysis, so some feature might be available only for GitLab. We try to update this documentation with all information as much as we can, do not hesitate to do a pull request if some feature are missing

## Import a model

Once the GitProjectHealth project has been loaded, one has to _import_ the project from the Git server to the Moose image.
As of today, we import a _group of repositories_.

### Import a group from GitLab

In a playground (`Ctrl+O`, `Ctrl+W`).

```st
glhModel := GLHModel new.

glhApi := GLHApi new
    privateToken: '<Your private token>';
    baseAPIUrl:'https://gitlab.myPrivateHost.com/api/v4';
    yourself.

glhImporter := GLHModelImporter new
    glhApi: glhApi;
    glhModel: glhModel.


"137 is the ID of the a Group, you can find the number in the webpage of every project and group"
glhImporter importGroup: 137.
```

### Import a group from GitHub

In a playground (`Ctrl+O`, `Ctrl+W`).

```st
glhModel := GLHModel new.

githubImporter := GHModelImporter new glhModel: glhModel; privateToken: '<my private token>'; yourself.

githubImporter importGroup: 'moosetechnology'.
```

### More commits extracted

![Badge Gitlab only](https://img.shields.io/badge/GitLab_Only-8A2BE2?logo=gitlab)

You might want to gather more commits for a specific repository.
To do so in GitLab, we added the following API

```st
myProject := ((glhModel allWithType: GLHProject) select: [ :project | project name = '<my projectName>' ]) anyOne.

glhImporter importCommitsOf: myProject withStats: true until: '2023-01-01' asDate.
```

## Visualize

To visualize the group's "health"

```st
dritGroup := (glhModel allWithType: GLHGroup) detect: [ :group | group id = 137 ].
canvas := (GLHGroupVisualization new forGroup: dritGroup).
canvas open.
```

## Export

To export the visualization as an svg image

```st
dritGroup := (glhModel allWithType: GLHGroup) detect: [ :group | group id = 137 ].
canvas := (GLHGroupVisualization new forGroup: dritGroup).
canvas open.

canvas svgExporter
  withoutFixedShapes;
  fileName: 'drit-group-health';
  export.
```
