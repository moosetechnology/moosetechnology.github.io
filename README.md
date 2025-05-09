# MooseTechnology web site - Blog and wiki <!-- omit in toc -->

- [Installation \& Setup](#installation--setup)
- [Moose Blog](#moose-blog)
  - [Create a draft](#create-a-draft)
    - [Add images and figures](#add-images-and-figures)
      - [MermaidJS](#mermaidjs)
      - [Drawio](#drawio)
    - [Add code block](#add-code-block)
  - [Add a TOC](#add-a-toc)
    - [Add/edit an author](#addedit-an-author)
    - [Test your draft locally](#test-your-draft-locally)
  - [Review a draft](#review-a-draft)
  - [External blog post](#external-blog-post)

## Installation & Setup

To install the website locally:

1. Install [jekyll](https://jekyllrb.com/docs/installation/).
2. Clone the project
3. Build your site and serve it: `bundle exec jekyll serve`

Assuming there are no errors and the site is building properly, the website is served at [localhost:4000](localhost:4000).

## Moose Blog

> All files must respect the kebabCase (HelloWorld -> hello-world. Lowercase separated by `-`)

### Create a draft

To create a new blog post, you should first create a draft that will be reviewed by other people.
To do so, create a new markdown file under the `_drafts`.
As a name, use the future publication date, or the today date, as prefix and then the post name.

For example, `2021-02-02-micro-visitors.md` is the post about `micro-visitors` and has been published on February 2, 2021.

Then, begin the file with the metadata

1. layout **must** be post
2. Title is the title of your post
3. Subtitle is a subtitle (by default, it will take the beginning of your post)
4. date is the publication date
5. background is the image you want as a background. If none, you **must** use `'/img/posts/bg-posts.jpg'`
6. author is your author name

Example in yaml:

```yml
---
layout: post
title: "Micro-Visitors for Parsing Programming Languages"
subtitle: My super nice subtitle
date: 2021-01-26 23:45:13 -0400
background: '/img/posts/bg-posts.jpg'
author: Nicolas Anquetil
tags: importers
---
```

Or in json:

```json
---
{
  "layout": "post",
  "title": "Micro-Visitors for Parsing Programming Languages",
  "subtitle": "My super nice subtitle",
  "date": "2021-01-26 23:45:13 -0400",
  "background": "/img/posts/bg-posts.jpg",
  "author": "Nicolas Anquetil",
  "comment": "true",
  "tags": ["importers"]
}
---
```

Then write the content of your post

#### Add images and figures

If you want to add an image or a figure, please add the file under `img/posts/<your-post-name>/myFile.png`.

You may want the figure to automatically scale with the post width.
To do so add the class `.img-fluid` to the image.

Example:

```md
!["My hint"](/img/posts/2021-02-04-Coasters/coastersUML.png){: .img-fluid }
```

The images are zoomable by default (the JavaScript code is under `/assets/lightense.min.js`).
If you need an image to not be zoomable, add the `.no-lightense` class.

Example:

```md
!["My hint"](/img/posts/2021-02-04-Coasters/coastersUML.png){: .no-lightense }
```

##### MermaidJS

If you use MermaidJS to draw diagram, they will automatically use dark/light theme

##### Drawio

Please to ensure that everybody can easily edit your schema made with drawio, use file extension `.drawio.png`.
So you can integrate the image easily, and everybody can open your file using drawio

#### Add code block

To add a code block, use the mardown format with

```smalltalk
'```smalltalk
myCode := 23
````'
```

### Add a TOC

It is possible to add automatically a toc to the right part of any article (blog post still need update).
To do so, you onyly have to add the toc option in the page header

```yml
---
toc: true
---
```

It is possible to omit a title by adding after the title the `{: .no_toc}` code

#### Add/edit an author

If it is the first time you write for the Moose blog, or if you want to edit your information, you might be interested by the `authors.json` file.
It includes your information in a Json format and is used to generate part of the blog post (the profile card for example).

To add an author or edit one, please edit the `_data/authors.json` file.
Example of existing fields are present in the file.
If you want to add other fields, please ask to the website maintainer.

```json
{
    "Nicolas Anquetil": {
        "name": "Nicolas Anquetil",
        "role": "Moose expert",
        "bio": "Software engineer researcher specialized in software evolution",
        "github": "https://github.com/nicolasanquetil",
        "image": "https://avatars.githubusercontent.com/u/14889146"
    },
    "Benoit Verhaeghe": {
        "name": "Benoit Verhaeghe",
        "role": "Cookie Lover",
        "bio": "PhD Student at Berger-Levrault on GUI migration",
        "github": "https://github.com/badetitou",
        "twitter": "https://twitter.com/badetitou",
        "image": "https://avatars.githubusercontent.com/u/6225039",
        "gravatar-hash": "ab4070086241400a4a428bf8994f05e0"
    }
}
```

> As for Benoit, you can use your gravatar-hash instead of a image.

#### Test your draft locally

When you ended to write your draft, you can start jekyll locally to visualize your draft as a post.
To do so execute:

```sh
bundle exec jekyll serve --draft
```


In addition, if you put a date that is future for the publication, you must add `--future` to your command to see your post appear.

To do so execute:

```sh
bundle exec jekyll serve --draft --future
```

### Review a draft

When someone have created a draft, we should review it before putting it live.
To do so, you can use common GitHub features.
Remember that the drafts are not displayed in the website (but can be seen in the GitHub repository).

When all reviews are done, move the draft file from `_drafts` folder to the `_posts` folder.
Then, `commit` and `push`.
GitHub compiles the website and put it live under few minutes.

### External blog post

To add an external blog post, add the `external_url` property in the yaml frontend.
The `external_site` is not use but might be in the future.

For example:

```yml
---
author: Benoit Verhaeghe
layout: post
title: "Integrate Software Engineering into the everyday world"
subtitle: "How Berger-Levrault build Moose visualisation and tools daily using CI"
date:  2023-09-05 09:00:00 +200
external_url: https://www.research-bl.com/integrate-software-engineering-into-the-everyday-world/
external_site: www.research-bl.com
---
```