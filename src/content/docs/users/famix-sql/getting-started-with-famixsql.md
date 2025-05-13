---
title: 'Getting started with FamixSQL'
---

This page is a guide on how to use Moose to analyze your relational database using FamixSQL.

## What is FamixSQL?

FamixSQL is a meta-model and a Famix importer that can be use to create a model of a database.
The FamixSQL importer relies on [ODBC](https://en.wikipedia.org/wiki/Open_Database_Connectivity) to create the model, thus, it is compatible with a wide range of RDBMS such as Oracle, PostgreSQL, and MSSQL.

The goal of FamixSQL is to give you tools to better understand your database, the dependencies between the tables, and the *health* of your schema.
To do so, it relies *critics* analysis using a first set of rules that one can extend with its specific needs.

## Installation

### Load FamixSQL

To install FamixSQL, you'll need a Moose10 or Moose11 image (see the [install Moose page](/moose-wiki/Beginners/InstallMoose)).

In the Moose image, in a playground (`Ctrl+O`, `Ctrl+W`), perform:

```smalltalk
Metacello new
  baseline: 'FamixSQL';
  repository: 'github://moosetechnology/FamixSQL:main/src';
  load.
```

This command will load the FamixSQL project as well as all its Pharo dependencies.

### Model Loading

To load a model from an ODBC connection, you first have to create the ODBC connection in your computer.
You can find plenty of tutorial on this subject on internet: [for Window](https://learn.microsoft.com/en-us/sql/integration-services/import-export-data/connect-to-an-odbc-data-source-sql-server-import-and-export-wizard?view=sql-server-ver16).

Once you have set up the odbc connection, you will be able to use it through Moose.
To do so, again in a playground, perform:

```smalltalk
connection := ODBCConnection new.
connection dsn: '<dsn name>'.
connection uid: '<username>'. "Username"
connection pwd: '<password>'. "Password"
connection connect.
```

> Replace value between `<>` by yours

This step create a connection object that will be use by the Famix importer.
You can now import perform the import by executing **in the same playground** the following script:

```smalltalk
importer := FamixSQLODBCImporter new.
"importer schema: '<my schema>'."
importer model: FamixSQLModel new.
importer source: connection.
model := importer import.

"Install in the Moose system"
model name: 'myDatabase'.
model installWithCache: false.
```

> Import step can take time. Be patient.

### Analyse the database

Once you have loaded the model, it is possible to analyze the database.
We describe here how to use the preconfigured analysis made using [Moose Critics](/blog/2022-08-08-moosecritics).

First, open the [Models Browser](/moose-wiki/Users/moose-ide/browsers#models-browser).
It shows a list of all the model loading in your Moose environment.
You should see the model `myDatabase` loaded in the previous step.

Then, open the Critics Browser.
In the critics browser, you will be able to load the predefined critics rules.
To do so, click on the `Import rules` button, then search in the file browser the file `db-critics.ston` that should be positioned near the FamixSQL git repository cloned in the first step. **By default,** it will be located under `<your home directory>/Pharo/images/<your image name>/pharo-local/iceberg/moosetechnology/FamixSQL/critics`.
You should see the list of rules in the middle panel of the Critics Browser.

You can now select your model in the Models Browser, and click on the `Propagate` button that will send your model to the Critics Browser.
In the Critics Browser, you should see your model entities in the left panel.

Finally, click on the `Run` button of the Critics Browser browser.
It will compute the rules and present the list of entities that raised an issue.
By *double-cliking* on an entity, you will be able to browse the violation using the [Moose Inspector](/moose-wiki/Users/moose-ide/browsers#moose-inspector)

## Additional analysis

Based on our work, we also built some additional analysis.
Some might already be integrated in the full Moose environment, and others are specific to this module.
Thus, we list some of our analysis that might help you analyzing your database.

### Visualization of the tables with their link

A Roassal3 visualization of the tables and their link is also available in this package and can be opened by executing the following script:

```smalltalk
tables := ((model allWithType: FamixSQLTable) first: 10) asMooseSpecializedGroup.

(SQLRSTableGroupBuilder new
  sourceGroup: tables;
  collapseAll;
  build;
  canvas) open.
```

### Table without primary key

To retrieve all tables without a primary key using a playground, you can perform the following script:

```smalltalk
(model allWithType: FamixSQLTable) select: [ :table | table columns noneSatisfy: #isPrimaryKeyColumn ]
```

## A bit more

### Raise an issue

If you have any trouble using FamixSQL, do not hesitate to [raise an issue](https://github.com/moosetechnology/FamixSQL/issues) or to contribute with a [pull request](https://github.com/moosetechnology/FamixSQL/pulls).

### Acknowledgement

Many thanks to Julien Deplanque who made the [first version of this work](https://github.com/juliendelplanque/FAMIXNGSQL).
