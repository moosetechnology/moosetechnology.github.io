---
layout: page
author: Benoit Verhaeghe
background: '/img/bg-wiki.jpg'
title: 'Moose Critics'
subtitle: 'Create your own critics on model'
toc: true
---

Moose Critics is a tool of MooseIDE that allows one to define a suite of rules to verify on software systems' models.
It can be compared to a tool to create rules such as the one provided by [SonarQube](https://www.sonarsource.com/products/sonarqube/).

We first present how to use the UI to manipulate the tool. Then we present how to use it programmatically.

## With the User Interface

The easiest way to manipulate the Critics Browser is to use its UI.

### Open the Moose Critics Browser

You can open the browser in the Menu under `Moose > Moose Critic Browser`.

The browser is divided into three main panels.
From left to right: the entities you are checking, the rules and context you want to check, and the computed violations.

The entities panels (left) grouped the entities by their type.
You can inspect any entity (or a group of entities) by double-clicking on its entry.

> If you do not see entities, you might not have loaded any model and propagate entities.
> Please consider reading MooseIDE documentation first.

You can now create Rules and context for your rules

### Create and Apply Rules

Rules in MooseCritics are divided into two components: Context, and Condition.
A context is a collection of entities to specify the scope of our analysis. Using this, we are only executing our rules on the relevant entities for them.
This step is crucial for performance reasons.
Once we have a context, we add conditions to it, to verify the validity of every entity belonging to this context.

Let’s start building a few of those, to appreciate how easy and versatile this system can be!

#### Create a context

To create your first context, right-click on `Root context` and select `Add context` entry.
It displays a panel in which you can specify the name of the context, an input, and the text summary (*a.k.a.,* description) of the context.

The input block takes as input all the entities of the parent context (`root context` provides all the entities) and should return a collection of entities.

For example, a context that would select all the methods would be filled with the following input block:

```st
[ :collection | collection select: [ :entity | entity isMethod ] ]
```

#### Create a rule

Once you have created your context, you can create rules.
To create your first rule, right-click on a context and select `Add condition` entry.
As for context, rules are composed of a name, an input block, and a description.
It also includes a severity level.

This time, the input block will take as a parameter each element of the collection of entities provided by the parent context.
The block should return a boolean: `true` when the entity violated the rule, otherwise, it returns `false`.

For example, to retrieve the dead methods, we would fill it like this:

```st
[ :entity | entity isDeadMethod ]
```

### Run the critics

Once the browser is configured, it is possible to execute the rules suite.
To do so, simply press the `Run` button at the bottom right of the window.
The right side panel will be filled with the entities that violated the rules.

### Export and Import Rules

When closing the window, the critic's suite will be removed from the system.
However, it is possible to save it for future usage.

To do so, we use the Ston format (similar to JSON but with support for complex object export).
To export the result, you only have to click on the `Export rules` button and select the destination folder.

The import is made the same way using the `Import rules` button.

## Use it programmatically

## Relative blog posts and documentation

- [First Blog Post]({% post_url 2022-08-08-moosecritics %})
- [Integration in a CI](https://www.research-bl.com/2023/09/05/integrate-software-engineering-into-the-everyday-world/)
