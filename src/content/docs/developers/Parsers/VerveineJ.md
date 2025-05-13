---
title: VerveineJ
---

VerveineJ is a tool written in java that create a *json* or a  _mse_ file from Java source code.

## Installation

To install VerveineJ, you need to clone it form [VerveineJ github repositiory](https://github.com/moosetechnology/VerveineJ).
You might prefer the [latest released version](https://github.com/moosetechnology/VerveineJ/releases).

```bash
# https
git clone https://github.com/moosetechnology/VerveineJ.git

# ssh
git clone git@github.com:moosetechnology/VerveineJ.git
```

## Usage

### Quick start

To use VerveineJ, you can use [Famix Maker](https://github.com/moosetechnology/Moose-Easy) ![External documentation](https://img.shields.io/badge/-External%20Documentation-blue) or from a terminal.

From the terminal on Unix systems, you can use command line as follows:

```sh
./verveinej.sh -o MyProject.mse -autocp ../MyProjectDependenciesOrLib/ ../MyProjectSrcFolder/
```

On Windows, you should use `verveinej.bat` instead of `verveinej.sh`.

To use the JSON file format, you can specify the output format of VerveineJ:

```sh
./verveinej.sh -o MyProject.json -format json -autocp ../MyProjectDependenciesOrLib/ ../MyProjectSrcFolder/
```

### Options

In the following, we describe the options of VerveineJ.

Usage:

`VerveineJ [-h] [-i] [-format (mse|json)] [-prettyPrint] [-o <output-file-name>] [-summary] [-alllocals] [-anchor (none|default|assoc)] [-cp CLASSPATH | -autocp DIR] [-1.1 | -1 | -1.2 | -2 | ... | -1.7 | -7] <files-to-parse> | <dirs-to-parse>"`

<table>
  <thead>
    <tr>
      <th>VerveineJ options</th>
      <th>description</th>
    </tr>
  </thead>
  <tbody>
    <tr><td><code>-h</code></td><td>prints this message</td></tr>
    <tr><td><code>-i</code></td><td>toggles incremental parsing on (can parse a project in parts that are added to the output file)</td></tr>
    <tr><td><code>-format (mse|json)</code></td><td>specifies the output format (default: MSE)</td></tr>
    <tr><td><code>-prettyPrint</code></td><td>toggles the usage of the json pretty printer</td></tr>
    <tr><td><code>-o &lt;output-file-name&gt;</code></td><td>specifies the name of the output file (default: ouput.mse)</td></tr>
    <tr><td><code>-summary</code></td><td>toggles summarization of information at the level of classes. Summarizing at the level of classes does not produce Methods, Attributes, Accesses, and Invocations. Everything is represented as references between classes: e.g. "A.m1() invokes B.m2()" is uplifted to "A references B".</td></tr>
    <tr><td><code>-alllocals</code></td><td>Forces outputting all local variables, even those with primitive type (incompatible with "-summary")</td></tr>
    <tr><td><code>-anchor (none|entity|default|assoc)</code></td><td>options for source anchor information: - no entity - only named entities [default] - named entities+associations (i.e. accesses, invocations, references)</td></tr>
    <tr><td><code>-cp CLASSPATH</code></td><td>classpath where to look for stubs</td></tr>
    <tr><td><code>-autocp DIR</code></td><td>gather all jars in DIR and put them in the classpath</td></tr>
    <tr><td><code>-filecp FILE</code></td><td>gather all jars listed in FILE (absolute paths) and put them in the classpath</td></tr>
    <tr><td><code>-excludepath GLOBBINGEXPR</code></td><td>A globbing expression of file path to exclude from parsing</td></tr>
    <tr><td><code>-1.1 | -1 | -1.2 | -2 | ... | -1.7 | -7</code></td><td>specifies version of Java</td></tr>
    <tr><td><code>&lt;files-to-parse&gt;|&lt;dirs-to-parse&gt;</code></td><td>list of source files to parse or directories to search for source files</td></tr>
  </tbody>
</table>

### Advanced Options

#### Dealing with accent in code

It is possible to parse code with accent using a Java vm option.

To do so, add the encoding before a double dash `--`. For example:

```sh
./verveineJ.sh -Dfile.encoding=ISO-8859-1 -- -format json <...>
```

## Using docker

It is also possible to use VerveineJ with [Docker](https://github.com/Evref-BL/VerveineJ-Docker).
Please look at the repository documentation.
