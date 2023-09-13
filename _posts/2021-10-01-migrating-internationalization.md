---
author: Benoit Verhaeghe
layout: post
title: "Migrating internationalization files"
subtitle: "A nice MDE example"
background: '/img/posts/bg-posts.jpg'
date:   2021-10-01 12:00:00 +200
---

During my Ph.D. [migration project](https://badetitou.fr/projects/Casino/), I considered the migration of several GUI aspects: 

- visual
- behavioral
- business

These elements are the main ones.
When *perfectly* considered, you can migrate the front-end of any application.
But, we are missing some other stuff :smile:
For example, how do you migrate i18N files?

In this post, I'll present how to build a simple migration tool to migrate I18N files from `.properties` (used by Java) to `.json` format (used by Angular).

## I18N files

First, let's see our source and target.

As a source, I have several `.properties` files, including I18N for a Java project.
Each file has a set of *key/value* and comments.
For example, the `EditerMessages_fr.properties` is as follow:

```properties
##########
#    Page : Edit
##########

pageTitle=Editer
classerDemande=Demande
classerDiffusion=Diffusion
classerPar=Classer Par
```

And it's Arabic version `EditerMessages_ar.properties`

```properties
#########
#    Page : Editer
#########

pageTitle=تحرير
classerDemande=طلب
classerDiffusion=بث
classerPar=تصنيف حسب
```

As a target, I need **only one JSON file** per language.
Thus, the file for the french translation looks like this:

```json
{
    "EditerMessages" : {
        "classerDemande" : "Demande",
        "classerDiffusion" : "Diffusion",
        "classerPar" : "Classer Par",
        "pageTitle" : "Editer"
    }
}
```

And the Arabic version:

```json
{
    "EditerMessages" : {
        "classerDemande" : "طلب",
        "classerDiffusion" : "بث",
        "classerPar" : "تصنيف حسب",
        "pageTitle" : "تحرير"
    },
}
```

To perform the transformation from the `.properties` file to `json`, we will use MDE.
The approach is divided into three main steps:

1. Designing a meta-model representing internationalization
2. Creating an importer of properties files
3. Creating a JSON exporter

## Internationalization Meta-model

I18N files are simple.
They consist of a set of *key/values*.
Each value is associated with a language.
And each file can be associated with a namespace.

For example, in the introduction example, the namespace of all entries is *"EditerMessages"*.

I designed a meta-model to represent all those concepts:

![meta-model](/img/posts/2021-10-01-Migrating-internationalization/I18N-metamodel.png)

Once the meta-model is designed, we must create an importer that takes `.properties` files as input and produces a model.

## Properties Importer

To produce a model, I first look for a `.properties` parser without much success.
Thus, I decided to create my own parser.
Given a correctly formatted file, the parser provides me the I18N entries.
Then, by iterating on this collection, I build an I18N model.

### I18N parser

To implement the parser, I used the [PetitParser2 project](https://github.com/kursjan/petitparser2).
This project aims to ease the creation of new parsers.

First, I downloaded the last version of [Moose](https://modularmoose.org/moose-wiki/Beginners/InstallMoose), and I installed PetitParser using the command provided in the repository Readme:

```st
Metacello new
    baseline: 'PetitParser2';
    repository: 'github://kursjan/petitparser2';
    load.
```

In my Moose Image, I created a new parser.
To do so, I extended the `PP2CompositeNode` class.

```st
PP2CompositeNode << #CS18NPropertiesParser
    slots: { };
    package: 'Casino-18N-Model-PropertyImporter'
```

Then, I defined the parsing rules.
Using PetitParser2, each rule corresponds to a method.

First, `start` is the entry point.

```st
start
    ^ pairs end
```

`pairs` parses the entries of the `.properties` files.

```st
pairs

    ^ comment optional starLazy, pair , ((newline / comment) star , pair ==> [ :token | token second ]) star , (newline/comment) star ==> [ :token | 
      ((OrderedCollection with: token second)
           addAll: token third;
           yourself) asArray ]
```

The first part of this method (before `==>`) corresponds to the rule parsed.
The second part (after `==>`), to the production.

The first part tries to parse one or several `comment`.
Then, it parses one `pair` followed by a list of `comment`, `newline`, and `pair`.

![](https://mermaid.ink/svg/eyJjb2RlIjoiZmxvd2NoYXJ0IExSXG4gICAgQ29tbWVudCAtLT4gUFtcIlBhaXJcIl1cbiAgICBQIC0tPiBDW1wiQ29tbWVudC9OZXcgTGluZVwiXVxuICAgIHN1YmdyYXBoIHN0YXJcbiAgICBDIC0tPiBQYWlyXG4gICAgZW5kXG4gICAgUGFpciAtLT4gRU5EXG4gICIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0In0sInVwZGF0ZUVkaXRvciI6ZmFsc2UsImF1dG9TeW5jIjp0cnVlLCJ1cGRhdGVEaWFncmFtIjpmYWxzZX0)

> This parser is clearly not perfect and would require some improvement.
> Nevertheless, it does work for our context.

The second part produces a collection (*i.e.* a list) of the `pair`.

### Building the I18N model

Now that we can parse one file, we can build a I18N model.
To do so, we will first parse every `.properties` file.
For each file, we extract the `language` and the `namespace` based on the file name.
Thus, `EditerMessages_fr.properties` is the file for the `fr` language and the `EditerMessages` namespace.
Then, for each file entry, we instantiate an entry in our model inside the namespace and with the correct language attached.

```st
importString: aString
    (parser parse: aString) do: [ :keyValue | 
        (self model allWithType: CS18NEntry) asOrderedCollection
            detect: [ :entry | 
                "search for existing key in the file"
                entry key name = keyValue key ]
            ifOne: [ :entry |
                "If an entry already exists (in another language for instance)"
                entry addValue: ((self createInModel: CS18NValue)
                    name: keyValue value;
                    language: currentLanguage;
                    yourself) ]
            ifNone: [
                "If no entry exist"
                (self createInModel: CS18NEntry)
                    namespace: currentNamespace;
                    key: ((self createInModel: CS18NKey)
                        name: keyValue key;
                        yourself);
                    addValue: ((self createInModel: CS18NValue)
                        name: keyValue value;
                        language: currentLanguage;
                        yourself);
                    yourself ] ]
```

After performing the import, we get a model with, for each namespace, several entries.
Each entry has a key and several values.
Each value is attached to the language.

## JSON exporter

To perform the JSON export, I used the [NeoJSON project](https://github.com/svenvc/NeoJSON).
NeoJSON allows one to create a custom encoder.

For the export, we first select a language.
Then, we build a dictionary with all the namespaces:

```st
rootDic := Dictionary new.
    (model allWithType: CS18NNamespace)
        select: [ :namespace | namespace namespace isNil ]
        thenDo: [ :namespace | rootDic at: namespace name put: namespace ].
```

To export a namespace (*i.e.*, a `CS18NNamespace`), I define a custom encoder:

```st
writter for: CS18NNamespace customDo: [ :mapper | 
    mapper encoder: [ :namespace | (self constructNamespace: namespace) asDictionary 
        ] 
    ].
```

```st
constructNamespace: aNamespace
    | dic |
    dic := Dictionary new.
    aNamespace containables do: [ :containable | 
        (containable isKindOf: CS18NNamespace)
            ifTrue: [ dic at: containable name put: (self constructNamespace: containable) ]
            ifFalse: [ "should be an CS18NEntry" 
                dic at: containable key name put: (containable values detect: [ :value | value language = language ] ifOne: [ :value | value name ] ifNone: [ '' ]) ] ].
    ^ dic
```

The custom encoder consists on converting a `Namespace` into a dictionary of entries with the entries keys and their values in the selected language.

## Perform the migration

Once my importer and exporter are designed, I can perform the migration.
To do so, I use a little script.
It creates a model of I18N, imports several `.properties` file entries in the model, and exports the Arabic entries in a JSON file.

```st
"Create a model"
i18nModel := CS18NModel new.

"Create an importer"
importer := CS18NPropertiesImporter new.
importer model: i18nModel.

"Import all entries from the <myProject> folder" 
('D:\dev\myProject\' asFileReference allChildrenMatching: '*.properties') do: [ :fileRef | 
    self record: fileRef absolutePath basename.
    importer importFile: fileRef.
].

"export the arabian JSON I18N file"
'D:/myFile-ar.json' asFileReference writeStreamDo: [ :stream |
    CS18NPropertiesExporter new
        model: importer model;
        stream: stream;
        language: ((importer model allWithType: CS18NLanguage) detect: [ :lang | lang shortName = 'ar' ]);
        export
]
```

## Ressource

The meta-model, importer, and exporter are freely available in [GitHub](https://github.com/badetitou/Casino-I18N).
