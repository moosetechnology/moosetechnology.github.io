---
title: 'Define a baseline loading Moose only if not present'
---

Moose is a big project and loading it each time can take a long time.

Multiple people came to me asking if there was a way in the [baseline](https://github.com/pharo-open-documentation/pharo-wiki/blob/master/General/Baselines.md#baselines) of a project to say "Load Moose only if needed".

Since it is a common question, here is a guide on how to do that!

One way to do it is to use [custom project attributes from Metacello](https://github.com/pharo-open-documentation/pharo-wiki/blob/master/General/Baselines.md#define-custom-attributes). The principle is simple; depending on the state of your image, you can define project attributes and use them in your baseline.

To define the attributes you need to implement a method `#customProjectAttributes` that will return an array with the custom attributes. 

If I want to ensure Famix is in the image I can define it like this:

```smalltalk
customProjectAttributes

	'BaselineOfFamix' asPackageIfAbsent: [ ^ #( #NeedsFamix ) ].
	^ #(  )
```

:::note[Note]
You need to adapt this to the project you depend on. Maybe you'll need to check the baseline of Moose instead of Famix, or a specifi package not present in the default famix.
:::

And now I can use those attributes in a baseline via the `#for:do:` message.

## Full example

```smalltalk
customProjectAttributes

	'BaselineOfFamix' asPackageIfAbsent: [ ^ #( #NeedsFamix ) ].
	^ #(  )
```

```smalltalk
baseline: spec
    <baseline>
    spec
        for: #common
        do: [ spec package: 'MyProject' ].

    spec
        for: #(#'NeedsFamix') do: [
            spec baseline: 'Famix' with: [ spec repository: 'github://moosetechnology/Famix/src' ]
            spec package: 'MyProject' with: [ spec requires: #('Famix') ] ]
```
