---
title: 'Local GitProject Health'
---

Using only API to load a GitRepository can be time consuming.
Another option is to load a cloned repository. 
This is the reason behing `GitLocalModelImporter` that allows one to load from an already cloned repository.

This approach is more efficient but comes with limitations.
The major one is that it cannot load information hosted by the git social platform: for example: Merge Request, Comment, Pipelines, are not loadable with this approach.

However it is much faster to load commits and commits diff.

## Clone git repository

To analyse a git repository, you simply have to clone it our your local computer.

If you need to analyse all the branches, you need a more deep clone.
One approach is to use this script

```sh
git clone gitrepo
cd gitrepo
for branch in `git branch -a | grep remotes | grep -v HEAD | grep -v master | grep -v main `; do
   git branch --track ${branch#remotes/origin/} $branch
done
```

## Usage

After loading the project, the easiest way to use the project is to use this code snippet:

```smalltalk
glhModel := GLHModel new.
repository := (GLHRepository new
  cacheAt: #localImporterReference
  put: '/path/to/repo'
    asFileReference;
  yourself).

glhModel add: repository.


localImporter := GitLocalModelImporter new.
localImporter withFiles: true.
localImporter glhModel: glhModel.
localImporter importRepository: repository.

glhModel
```
