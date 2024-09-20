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

You can also import all available top-level groups and then iterate:

```st
groups := glhImporter importAllGroups.
groups do: [:group | 
  glhImporter importGroup: group id.
]
```

### Import a group from GitHub

In a playground (`Ctrl+O`, `Ctrl+W`).

```st
glhModel := GLHModel new.

githubImporter := GHModelImporter new
  glhModel: glhModel;
  privateToken: '<my private token>';
  yourself.

githubImporter importGroup: 'moosetechnology'.
```

### Extract repository commits


You might want to gather commits for repositories.
Instead of loading all commits and for performance reason, we propose to load commits since a define date. To do so, you can use the API

```st
myImporter withCommitsSince: (Date today - 100 days).
```

Remember to set this option before the import of project.

### Import Merge Requests information

![Badge Gitlab only](https://img.shields.io/badge/GitLab_Only-8A2BE2?logo=gitlab)

It is possible to extract MergeRequests of a project.
To avoid too many REST API requests, you should do it in multiple steps.

First, import basic information.

```st
(glhModel allWithType: GLHProject) do: [:project | 
  glhImporter importMergeRequests: project
]
```

> This will only import the first 20 merge requests for each project. However, one can also use the method `importMergeRequests:since:until:` to import all merge request since a specific date

Then, you can import more data for each MergeRequest

```st
(glphModel allWithType: GLPHEMergeRequest) do: [ :mr |
  "Extract information about Merge Request approvals and review"
  glhImporter importMergeResquestApprovals: mr.
  "Extract information about Author, Mergers, etc."
  glhImporter importMergeResquestAuthor: mr.
].
```

## Visualize

To help with your first analysis of repositories and organization, we included some basic visualization.

### Organization visualization

When using the Pharo inspector to inspect a group, you'll get a new tab called `Group quality`.
It shows you the repositories and if they are pipelines working for them.

You can also have a look at this visualization using the following script. 

```st
dritGroup := (glhModel allWithType: GLHGroup) detect: [ :group | group id = 137 ].
canvas := (GLHGroupVisualization new forGroup: { dritGroup } ).
canvas open.
```

#### Export

To export the visualization as an svg image

```st
dritGroup := (glhModel allWithType: GLHGroup) detect: [ :group | group id = 137 ].
canvas := (GLHGroupVisualization new forGroup: {dritGroup}).
canvas open.

canvas svgExporter
  withoutFixedShapes;
  fileName: 'drit-group-health';
  export.
```

### Commits distribution

We integrated a visualization that shows the commits distribution on a project by date.
Commits are also grouped by User who committed.

The same visualization is available in a user centric way, grouped by contributed project.

![Commits distribution for a user](img/commits-distribution.png){: .img-fill }

## Meta-model

The meta-model of GitProjectHealth is automatically update by the FamixUMLDocumentor.

![Image of the metamodel](https://raw.githubusercontent.com/moosetechnology/GitProjectHealth/v1/doc/gitproject.png){: .img-fill }

It includes several entities.
The main one is probably *Commit* with all its attributes as well as *User*.

## Connect with others meta-model

It is possible to connect with others meta-model.
In here, we describe briefly how to do it.

### Jira Connector


The Jira connector connect this project to the [Pharo Jira API project](https://github.com/Evref-BL/Jira-Pharo-API).
It basically looks for commit and merge request links to Jira Issue.

To install the connector, please perform:

```st
Metacello new
  repository: 'github://moosetechnology/GitProjectHealth:main/src';
  baseline: 'GitLabHealth';
  onConflict: [ :ex | ex useIncoming ];
  onUpgrade: [ :ex | ex useIncoming ];
  onDowngrade: [ :ex | ex useLoaded ];
  load: #( 'default' 'Jira' )
```

> loading default is optional if you already loaded it.

Then, it is possible to connect two models using

```st
GPJCConnector new
  gpModel: aGpModel; "or glh model"
  jiraModel: aJiraModel;
  connect
```

### Famix Connector

> You will probably have to learn what Famix is.
> Lucky, you are in the website of the Famix documentation 😄.

To connect with a Famix, first import a FamixModel using VerveineJ or any other tools.
Then, load the Famix connector of GitProjectHealth with the following script:

```st
Metacello new
  repository: 'github://moosetechnology/GitProjectHealth:main/src';
  baseline: 'GitLabHealth';
  onConflict: [ :ex | ex useIncoming ];
  onUpgrade: [ :ex | ex useIncoming ];
  onDowngrade: [ :ex | ex useLoaded ];
  load: #( 'default' 'Famix' )
```

Then, it is possible to connect two models using

```st
GPCGitToFamixConnector new
  famixModel: myFamixModel;
  glhProject: myGitProject;
  connect
```

By connecting a GitProject model with Famix, you will get access to the `appliedDiffs` method on `FamixTClass`.
You will also be able to navigate from a Git diff to the current entity in the model (if it exists).


