---
title: FAST
---

FAST means *Famix AST*.
Contrary to Famix that represent application at a high abstraction level, FAST uses a low-level representation: the AST.

From Moose 9, and version *v2* of FAST, FAST defined a set of traits that can be used to create new meta-models compatible with Moose tools.
This strategy is similar to the [pre-defined traits of Famix](/developers/predefinedentities).

With version *v3*, FAST also provides importers: they turn source files into FAST models, and an agnostic importer chooses the right importer for each file.

## The pieces

- **The FAST meta-model** (the `FAST` repository): generic traits (`FASTT*`) and entities (`FAST*Entity`) shared by all the languages.
- **The language projects** (FAST-JAVA, FAST-Python, FAST-CSS, ...): one project per language, with its own meta-model generated from the language grammar, and its importer.
- **The importers**: classes that read a source file and answer a FAST model of that file. Tree-sitter based importers are built on [Pharo-Tree-Sitter](https://github.com/Evref-BL/Pharo-Tree-Sitter), the others use their own parser (SmaCC for Java).
- **The importer descriptions**: one per language project, telling where the project is and which file extensions it handles. They let FAST load a language project that is not in the image yet.

## Install FAST

Execute in a playground:

```st
Metacello new
  githubUser: 'moosetechnology' project: 'FAST' commitish: 'v3' path: 'src';
  baseline: 'FAST';
  load: 'core'
```

The groups are `core` (the meta-model), `tools` (the tools and the importers) and `All` (everything, including the deprecated code). `core` and `tools` are what the continuous integration loads.

## Import a file

There are two ways to import a file.

**Use the importer of your language directly** when you know the language.
Each language importer has the same API, so you call it on its class:

```st
FASTJavaSmaCCImporter parseFile: 'src/Main.java' asFileReference.
```

**Use the agnostic importer** when you have files of several languages, or do not want to choose the importer.
It chooses the importer of each file from its extension, and it can load the project of a language that is not in the image yet:

```st
FASTAgnosticImporter parseFile: 'src/Main.java' asFileReference.
```

Add a file to a model you already have, to import several files in one model:

```st
| model |
model := FASTAgnosticImporter parseFile: 'src/Main.java' asFileReference.
FASTAgnosticImporter parseFile: 'src/Other.java' asFileReference into: model.
```

Ask which importer handles a file, without importing it:

```st
FASTAgnosticImporter importerFor: 'src/Main.java' asFileReference.
```

### Import a folder or a list of files

`parseFolder:` imports the files of a folder and its subfolders, and `parseFiles:` imports a list of files.
Both group the files by importer: each language gets one model.
They answer a dictionary from each model to the files that filled it.
The files that no importer handles are answered under `nil`:

```st
| result |
result := FASTAgnosticImporter parseFolder: 'src' asFileReference.
result keysAndValuesDo: [ :model :files |
	Transcript show: files size printString; cr ].
result at: nil.
```

### Missing importers

When no loaded importer handles the extension of a file, FAST can load the project of the matching description.
The strategy decides whether it may:

| Strategy | Behavior |
|---|---|
| `FASTAskOrLoadStrategy` (default) | In an interactive image, asks the user first; otherwise loads the project. |
| `FASTAlwaysLoadStrategy` | Loads the project without asking. |
| `FASTNeverLoadStrategy` | Never loads the project. The files that need it are answered under `nil`. |

The strategy is set on an instance of the agnostic importer, and the same messages are then used:

```st
FASTAgnosticImporter new
	missingImporterStrategy: FASTAlwaysLoadStrategy new;
	parseFolder: 'src' asFileReference.
```

Loading a project needs network access.

### Errors

- If several importers handle the extension of a file, `FASTAmbiguousImporterError` is signaled.
- If no importer handles a file in a single-file import, an error is signaled. A folder or list import answers such files under `nil` instead.

## Languages

| Language | Project | Parser | Extensions | Status |
|---|---|---|---|---|
| Java | [FAST-JAVA](/users/ast/fast-java) | SmaCC | `java` | Works |
| Python | [FAST-Python](/users/ast/fast-python) | Tree-sitter | `py` | Works |
| CSS | FAST-CSS | Tree-sitter | `css` | Works |
| HTML | FAST-HTML | Tree-sitter | `html` | Works |
| SCSS | FAST-SCSS | Tree-sitter | `scss` | Works |
| XML | FAST-XML | Tree-sitter | `xml` | Works |
| YAML | FAST-YAML | Tree-sitter | `yml` | Works |
| TypeScript | FASTTypescript | Tree-sitter | `ts` | Works |
| Pharo | [FAST-Pharo](/users/ast/fast-pharo) | Pharo parser | `st` | Not working since Moose 8 |
| Fortran | FAST-Fortran | FAST v2 | `f` | Not working with FAST v3 |

The projects that do not work are listed, but `FASTImporterDescription loadAll` does not load them.

## Add a language

To support a new language, you write its importer, and you describe its project to FAST.

1. **Create the language project** (its own baseline, meta-model and importer). See [Dealing with a new language](/developers/dealing-with-a-new-language).
2. **Write the importer.** It uses the `FASTTImporter` trait on its instance side and on its class side. It implements:
   - `parse:into:` on the instance side: import the source into the model given (a `nil` model means a new one), and answer the model.
   - `extensions` on the class side: the file extensions without the dot.

   The importers are found through the classes that use the trait, so there is nothing to register.
3. **Describe the project.** Add a `FASTImporterDescription` subclass in the `FAST-Importer` package, one per language. This step is required: without it, FAST cannot load the project when the language is not in the image, and `FASTAgnosticImporter` fails with *No importer handles the file*.

```st
FASTImporterDescription subclass: #FASTJavaImporterDescription.

FASTJavaImporterDescription class >> baselineName [ ^ 'FASTJava' ]
FASTJavaImporterDescription class >> projectName [ ^ 'FAST-JAVA' ]
FASTJavaImporterDescription class >> group [ ^ 'smacc' ]
FASTJavaImporterDescription class >> branch [ ^ 'v4' ]
FASTJavaImporterDescription class >> extensions [ ^ #( 'java' ) ]
```

A description gives:
- `baselineName`: the name of the Metacello baseline of the language project.
- `projectName`: the name of the GitHub repository of the project.
- `group`: the Metacello group to load.
- `branch`: the branch or tag to load. The default is `master`.
- `extensions`: the file extensions handled by the importer. They must match the extensions of the importer, since the agnostic importer uses them to find the project to load.
- `isWorking`: `true` by default. Answer `false` while the project does not work, so that `loadAll` skips it.

A description is loaded when an importer for one of its extensions is present in the image.

Check the description with a file of your language:

```st
FASTAgnosticImporter importerFor: 'src/Main.java' asFileReference.
FASTJavaImporterDescription isProjectLoaded.
```

## Incremental parsing

The Tree-sitter importers can reparse a file after a change, from the tree of its previous version.
Use `parse:usingOldTree:` on the model returned by `parse:`:

```st
| model |
model := importer parse: 'x = 1'.
newModel := importer parse: 'x = 2' usingOldTree: model.
```

Only the parse is incremental: the FAST model is rebuilt from the new tree.

## FAST meta-model

![FAST Meta-model](https://raw.githubusercontent.com/moosetechnology/FAST/v2-doc/fast-core.png)
