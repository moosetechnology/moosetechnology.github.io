---
layout: page
author: Benoit Verhaeghe
background: '/img/bg-wiki.jpg'
title: 'Moose Entities Exporter'
subtitle: 'Export Moose Metrics'
toc: true
---

Once you have performed many query to search for entities of interest, you might want to export the result in a file to be consumed by others software systems.
Moose IDE comes with the Moose Entities Exporter that helps you doing so.
It is possible to use the Moose Entities Browser to generate CSV using the User Interface, or programmatically by using the browser model.

## Export using the UI

## Export programmatically

Sometimes, it is easier to perform the export using programmatic approach.
It is for example the case when you want to perform export from a CI.

### Initialized your exporter

First, you will need to create a `MiExportModel` and to configure it with the data you will want to explore.

```st
exportBrowserModel := MiExportModel new.
exportBrowserModel entitiesList: collectionOfEntities.
```

Then, you might want to remove the default columns of the `MiExportModel`: `#Type` and `#Name`.
`#Type` is the string representing the type of the entity in the model, and `#Name` its moose name if some.
To remove them, simply send the `removeColumnForQueryNamed:` message to your `MiExportModel`.

```st
"Remove default column"
exportBrowserModel removeColumnForQueryNamed: #Name.
exportBrowserModel removeColumnForQueryNamed: #Type.
```

### Add column based on a custom based query

Once you have initialized your model, you can create column that will be computed based on a block.
The block will be computed for each entity of your `collectionOfEntities`.

```st
exportBrowserModel addColumnForQuery: [ :violation | violation violatedCondition name ] withName: #'Rule name'.
exportBrowserModel addColumnForQuery: [ :violation | violation violatedCondition summary ] withName: #'Rule summary'.
```

> The example above is done in combination with the Moose Critics tool to export information about Code violation.

### Export

The final step is the export of the model using the defined query.
We implemented two exporters: CSV and Markdown.

For the CSV exporter use the method `writeCSVOn:`, whereas for markdown use the methods `writeMarkdownTableOn:`.

Example:

```st
'D:/myFile.csv' asFileReference writeStreamDo: [ :aStream |
    "Note this line that can be added to easily open CSV file using Excel" 
    aStream << 'sep=,'; << OSPlatform current lineEnding.
    "Do export"
    exportBrowserModel writeCSVOn: aStream ].
```
