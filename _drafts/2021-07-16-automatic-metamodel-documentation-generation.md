---
author: Benoît Verhaeghe
layout: post
title: "Automatic meta-model documentation generation"
subtitle: "How to use GitHub actions to update its meta-model UML?"
date:  2021-07-16 08:00:00 +200
update_date:  2021-07-16 08:00:00 +200
comments: true
---

When you are developing with Moose everyday, you know how to create a nice visualisation of your meta-model.
But, we have to open a Pharo image, and it is hard to share it during a presentation.
Often, we made one UML of the meta-model, and then... we forget to update it.
Then, when sharing with others, you have to say that the UML is not correct but it is *ok???*.

In my opinion, this is **super bad**.
Thus, I decided to have a look at [GitHub Actions](https://fr.github.com/features/actions) to update my UML automatically.

In the folowing, I present you how to update your GitHub Action to add UML auto-generation.
Please consider reading the [blog post about how to use GitHub action with Pharo](https://badetitou.github.io/misc/pharo/2020/11/30/Testing-pharo-with-github-actions/).

