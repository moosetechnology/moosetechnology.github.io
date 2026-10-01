---
title: 'Mixed Importer'
---

GitProject Health is useful to analyze both *git repository* and the *social platform* information.
However, analysing git repository using the git platform (*e.g.,* GitHub, GitLab, BitBucket, ...) public REST API is time and energy consuming.
The easy solution is to use the local importer. However, this importer lacks the capability to retrieve information about Merge Request, Pipelines status, and so on.

To tackle down this issue, GitProjectHealth comes with a mixed importer.
The mixed importer import all the information relatives to the git repository using the local importer, and the one relative to social from the API of the social platform provider.

## Usage

Using the mixed importer is easy. You simply have to configure your remote importer (or repo importer), and then you can configure a mixed importer as a normal one.

```smalltalk
glphModel := GLHModel new name: 'mixed'.

glphApi := GitlabApi new
    privateToken: #'<token>';
    hostUrl: '<url>';
    output: 'json';
    yourself.


glhImporter := GitlabModelImporter new repoApi: glphApi.
localImporter := GitLocalModelImporter new.


mixedImporter := GitModelMixedImporter new.
mixedImporter apiImporter: glhImporter.
mixedImporter localImporter: localImporter.
mixedImporter glhModel: glphModel.
mixedImporter withCommitDiffs: true.
localImporter withDiffRanges: true.
```

> In case of error in the local importation, the mixed importer will rely on the *api* one to perform the importation
