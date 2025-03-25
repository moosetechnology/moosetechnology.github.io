---
layout: post
title: "Creating a Parser based on Tree-Sitter grammar"
date: 2025-03-25
background: '/img/posts/bg-posts.jpg'
author: Nicolas Anquetil
comments: true
tags: infrastructure
---

Moose is a huge consumer of programming language grammars.
We are always looking into integrating new programming languages into the platform.
There are two main requirements for this:
- create a parser of the language, to "understand" the source code
- create a meta-model for the language, to be able to represent and manipulate the source code

Creating the meta-model has already been coverred in an other blogpost: [https://modularmoose.org/posts/2021-02-04-coasters](https://modularmoose.org/posts/2021-02-04-coasters)

In this post, we will be looking at how to use a Tree-Sitter grammar to help build a parser for a language.
We will use the Perl language example for this.

Note: Creating a parser for a language is a large endehavour that can easily take 3 to 6 months of work.
Tree-Sitter, or any other grammar tool, will help in that, but it remains a long task.

## Getting the Tree-Sitter grammar

We do not explain in detail here how to install tree-sitter or a new Tree-Sitter grammar.
I found this page ([https://dcreager.net/2021/06/getting-started-with-tree-sitter/](https://dcreager.net/2021/06/getting-started-with-tree-sitter/)) useful in this sense.

For this blog post, we use the grammar in [https://github.com/tree-sitter-perl/tree-sitter-perl](https://github.com/tree-sitter-perl/tree-sitter-perl).
Do the following:
- clone the repository on your disk
- go in the directory
- do `make` (note: it gave me some error, but the library file was generated all the same)
- (on Linux) it creates a `libtree-sitter-perl.so` dynamic library file.
  This must be moved in some standard library path (I chose `/usr/lib/x86_64-linux-gnu/` because this is where the `libtree-sitter.so` file was).
  
Pharo uses FFI to link to the grammar library, that's why it needs to be in a standard directory.
The subclasses of `FFILibraryFinder` can tell you what are the standard directories on your installation.

For example on Linux, `FFIUnix64LibraryFinder new paths` returns a list of paths that includes `'/usr/lib/x86_64-linux-gnu/'` where we did put our grammar.so file.

## Binding tree-sitter in Pharo

We use the Pharo-Tree-Sitter project ([https://github.com/Evref-BL/Pharo-Tree-Sitter](https://github.com/Evref-BL/Pharo-Tree-Sitter)) of Berger-Levrault, created by Benoit Verhaeghe, a regular contributor to Moose and this blog.
You can import this project in a Moose image following the README instructions.
```st
Metacello new
  baseline: 'TreeSitter';
  repository: 'github://Evref-BL/Pharo-Tree-Sitter:main/src';
  load.
```

The README file of Pharo-Tree-Sitter gives an example of how to use it for Python:
```st
parser := TSParser new.
tsLanguage := TSLanguage python.
parser language: tsLanguage.
[...]
```

We want to have the same thing for Perl, so we will need to define a `TSLanguage class >> #perl` method.
Let's have a look at how it's done in Python:
```st
TSLanguage class >> #python
	^ TSPythonLibrary uniqueInstance tree_sitter_python
```

It's easy to do something similar for perl:
```st
TSLanguage class >> #perl
	^ TSPerlLibrary uniqueInstance tree_sitter_perl
```
But we need to define the `TSPerlLibrary` class.
Again let's look at how it's done for Python and copy that:
- create a `TreeSitter-Perl` package
- create a `TSPerlLibrary` class in it inheriting from `FFILibrary`
- define the class method:
```st
tree_sitter_perl
	^ self ffiCall: 'TSLanguage * tree_sitter_perl ()'
```
- and define the class methods for FFI (here for Linux):
```st
unix64LibraryName
	^ FFIUnix64LibraryFinder findAnyLibrary: #( 'libtree-sitter-perl.so' )
```
Notice that we gave the name of the dynamic library file created above (`libtree-sitter-perl.so`).
If this file is in a standard library directory, FFI will find it.


## A first Pharo AST

We can now experiment "our" parser on a small example:
```st
parser := TSParser new.
tsLanguage := TSLanguage perl.
parser language: tsLanguage.

string := '# this is a comment

my $var = 5;
'.

tree := parser parseString: string.

tree rootNode
```
This gives you the following window:

!["A first Tree-Sitter AST for Perl"](/img/posts/2025-03-25-tree-sitter/first-AST.png)

That looks like a very good start!

But we are still a long way from home.
Let's look at a node of the tree for fun.

`node := tree rootNode firstNamedChild` will give you the first node in the AST (the comment).
If we inspect it, we see that it is a `TSNode`
- we can get its type: `node type` returns the string `'comment'`
- `node nextSibling` returns the next TSNode, the "expression-statement"
- `node startPoint` and `node endPoint` tell you where in the source code this node is located.
  It returns instances of `TSPoint`:
  - `node startPoint row` = 0 (0 indexed)
  - `node startPoint column` = 0
  - `node endPoint row` = 0
  - `node endPoint column` = 19
  
  That is to say the node is on the first row, extending from column 0 to 19. 
  With this, one could get the text associated to the node from the original source code.
  
That's it for today.
In a following post we will start to look at how to create a real parser using on the Visitor design pattern.
