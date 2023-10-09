---
layout: post
title: "Introducing the new Queries Browser"
header-img: img/posts/DSM.jpg
subtitle: >-
  Here I am going to show you the new Queries Browser!
background: '/img/posts/bg-posts.jpg'
date: 2021-10-10 11:00:00
author: Sebastian Jordan
comments: true
tags: browser
---

## What is a Famix Query?

Let's say that you want to know which classes of your Moose model are stub.
That means: which classes are not defined in your Moose model but are used by some of your defined classes.
Those classes are part of your model, although they are not part of your code.
Checking those stub classes is easy.
You only have to create a Boolean query (assuming that your model contains only classes) with the property `isStub`.
Like:

```st
FQBooleanQuery property: #isStub
```

However, creating queries programmatically can be a tedious task.
Of course, not all the queries are as frivolous as the one in the example.
For queries with lots of children, the code is not easy to understand at first sight.

## Queries Browser

The new Queries Browser was developed to create queries in a more visual way.
This is a Moose tool that allows one to create and manipulate queries without the need of knowing the FamixQueries syntax or how to instantiate them.
It has a friendly and intuitive user interface.

!["The brand new Queries Browser"](/img/posts/2021-07-27-the-new-queries-browser/and-query.png){: .img-fill }

If you want to know:

1. What are all the classes that are not stub.
2. What are the entities with incoming references and inheritances.
3. What are the entities that have more than 50 lines of code.
4. Finally, what is the intersection of those three queries.

This is an easy task for the Queries Browser. First, we need to create a type query that filters all the entities except the classes.
To do that, we select Type Query in the queries browser and then select type "Classes".

!["Type Query"](/img/posts/2021-07-27-the-new-queries-browser/type-query.png){: .img-fill }

Then, we create a child query from the Type Query. This child is going to be a Boolean Query that has the property `isStub`.

!["The brand new Queries Browser"](/img/posts/2021-07-27-the-new-queries-browser/child-of-type-query.png){: .img-fill }

Now, we create a Complement Query, a.k.a Negation Query, and choose the Boolean Query to be the query to be negated.

!["The brand new Queries Browser"](/img/posts/2021-07-27-the-new-queries-browser/negation-query.png){: .img-fill }

Now we have the first task completed: All the classes that are not stub.

For the second task, we need to create another query, a Navigation Query, select the `Incoming` direction, and only select the associations `Reference` and `Inheritance`.

The third one is also simple.
We only need to create a Numeric Query, select the property number of lines of code, the operator `>`, and put the value 50.

Now, our Queries Browser looks like this:

!["The brand new Queries Browser"](/img/posts/2021-07-27-the-new-queries-browser/numeric-query.png){: .img-fill }

For the final task, we need to create an And Query, a.k.a Intersection Query, click on the "+" button to add a new query to intersect, and select the three previous queries that we created.

!["The brand new Queries Browser"](/img/posts/2021-07-27-the-new-queries-browser/and-query.png){: .img-fill }

If we wanted to the the above queries programmatically, the code would have look like this:

```st
(FQComplementQuery queryToNegate:
	 (FQTypeQuery types: { FamixStClass })
	 --> (FQBooleanQuery property: #isStub))
& (FQNavigationQuery incoming associations: {
		 FamixStReference.
		 FamixStInheritance }) & (FQNumericQuery
	 property: #numberOfLinesOfCodeWithMoreThanOneCharacter
	 comparator: #>
	 valueToCompare: 50)
```

The code is also shown in the "Current query code tab":
!["The brand new Queries Browser"](/img/posts/2021-07-27-the-new-queries-browser/current-code.png){: .img-fill }

## Saving and loading the queries

As you may already noticed, there are two button for saving and loading the queries. The save button saves all queries that are currently present on the queries browser as a STON file. The queries will be saved inside a folder in the same location as the image. The path is determinated in `MiSaveQueriesCommand class>>#path`.

When loading the queries, the saved queries will be put after the queries that are present in the browser, if any. For example, if we save the queries that we have created above.

!["Save queries as"](/img/posts/2021-07-27-the-new-queries-browser/save-queries.png){: .img-fill }

Then, with an empty browser, we create a new query and then loading the file we get:

!["Load queries"](/img/posts/2021-07-27-the-new-queries-browser/load-queries.png){: .img-fill }

## Conclusion

The new Queries Browser can simplify how the Famix Queries are created and make it more visual and understandable.
Even if the example in this post is not complex, it made our tasks easier.
Analyzing models with real-life examples can lead to very nested queries.
Create those queries programmatically can be very tedious and error-prone.
The Queries Browser is here to help us in those cases. :smile:
