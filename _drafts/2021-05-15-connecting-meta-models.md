---
layout: post
title: "Connecting/Extending meta-models"
date: 2021-05-15 12:00:00 -0400
background: '/img/posts/2021-05-15-connecting-meta-models/bg-post.png'
author: Benoît Verhaeghe
comments: true
---

Sometimes, a model does not have all the information you want.
Or, you want to connect it with another one.
A classic example is going from an abstract level to another.

In this blog post, I will show you how to extend and connect a meta-model with another.
We will use the [Coaster example]({% post_url 2021-02-04-Coasters %}).

## Extending the Coaster meta-model

The Coaster meta-model is super great (I know... it is mine :smile: ).
Using it, one can manage its collection of coasters.

However, did you notice that there is only one kind of Creator possible: the brewery.
It is not great because some coasters are not created by breweries but for events.
My model is not able to represent this situation.
So, there are two possibilities: I can fix my meta-model, or I can extend it with a new concept.
Here, we will see how I can extend it.

![Extended Coaster meta-model](/img/posts/2021-05-15-connecting-meta-models/extended-coaster-model.drawio.svg){: .img-fill }


## Connecting meta-models

## And for Famix
