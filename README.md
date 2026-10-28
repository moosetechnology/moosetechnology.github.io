# Modular moose website

## 🚀 Project Structure

Inside of your project, you'll see the following folders and files:

```txt
.
├── public/
├── src/
│   ├── assets/
│   ├── components/
│   ├── content/
│   │   └── docs/
│   │       ├── about/authors.json
│   │       └── blog/
│   ├── utils/
│   └── content.config.ts
├── astro.config.mjs
├── package.json
└── tsconfig.json
```

Folder `src/content/docs/blog/` is used for the blog posts.

Other folders are used for classic website navigation.

Starlight looks for `.md` or `.mdx` files in the `src/content/docs/` directory. Each file is exposed as a route based on its file name.

Images can be added to `src/assets/` and embedded in Markdown with a relative link.

Static assets, like favicons, can be placed in the `public/` directory.

## Installation & Setup

To install the website locally:

1. Install Node
2. Execute `npm install`
3. Execute `npm run dev`

### 🧞 Commands

All commands are run from the root of the project, from a terminal:

| Command                   | Action                                           |
| :------------------------ | :----------------------------------------------- |
| `npm install`             | Installs dependencies                            |
| `npm run dev`             | Starts local dev server at `localhost:4321`      |
| `npm run build`           | Build your production site to `./dist/`          |
| `npm run preview`         | Preview your build locally, before deploying     |
| `npm run astro ...`       | Run CLI commands like `astro add`, `astro check` |
| `npm run astro -- --help` | Get help using the Astro CLI                     |

## Authoring

We are using the [Starlight](https://starlight.astro.build/components/steps/) with many components that can be used directly in markdown!
Do not hesitate to use them.

### LLM-friendly documentation

The Starlight `starlight-llms-txt` plugin generates `llms.txt`, `llms-full.txt`, and `llms-small.txt` from the documentation pages during the site build. These files are served from the site root to help LLMs discover and read the documentation.

### Adding a new docs page

When adding a new wiki page, you must link the page in the [astro.config.mjs](astro.config.mjs) under the `starlightSidebarTopics` item.
It allows one to organize the page.

> There are automatic way to import all pages, but they sort files by name, and sometimes we do not want that.

## Authoring a blog post

1. Create a new file under `src/content/docs/blog/`, named `YYYY-MM-DD-short-title.md` (or `.mdx` to use Starlight components)
2. Add the frontmatter

```yaml
---
authors:
- BenoitVerhaeghe
title: "Coasters collection"
description: "How to create a minimal model in Moose"
date: 2021-02-15
lastUpdated: 2021-03-04
tags:
- meta-model
cover:
  image: "./img/posts/2021-02-04-Coasters/background.jpg"
  alt: ""
series: "Famix Tools"
---
```

| Field         | Required | Usage                                                                                      |
| :------------ | :------- | :----------------------------------------------------------------------------------------- |
| `authors`     | yes      | Keys of [`authors.json`](src/content/docs/about/authors.json) (see [Authors](#authors))     |
| `title`       | yes      |                                                                                            |
| `date`        | yes      | Publication date, used to sort the posts. Keep it consistent with the file name            |
| `description` | no       | One sentence, used for SEO and link previews (not displayed on the page)                   |
| `lastUpdated` | no       | Date of the last significant update                                                        |
| `tags`        | no       | See [Tags](#tags)                                                                          |
| `cover`       | no       | Image displayed at the top of the post and in the blog list                                |
| `series`      | no       | Name of a series of posts. All posts sharing the same name are listed in a box at the top |

> Fields from the former Jekyll blog (`subtitle`, `background`, `header-img`, `comments`) are ignored by Starlight: do not use them.

Images of a post go in `src/content/docs/blog/img/posts/<file name of the post>/` and are referenced with a relative link.

### Tags

Reuse an existing tag before creating a new one, and respect its exact spelling (tags are case-sensitive).

| Tag                           | Usage                                                         |
| :---------------------------- | :------------------------------------------------------------ |
| `meta-model`                  | Famix meta-models, their design and evolution                 |
| `Famix-tools`                 | Tools working on Famix models (visitors, UML, queries…)       |
| `FAST`                        | FAST models (AST) and the tools on top of them               |
| `infrastructure`              | Parsers, importers, and Moose internals                       |
| `browser`                     | Moose browsers and UI (Spec)                                  |
| `CI`                          | Running Moose in a continuous integration                     |
| `Mining Software Repositories`| Analysis of the history of a project (git, GitProjectHealth)  |
| `transformation`              | Code transformation                                           |
| `story`                       | Real use case of Moose on a project                           |
| `Python`, `Java`              | Language-specific posts                                       |

### Series

When writing several posts on the same topic, give them the same `series` value.
A box listing all the posts of the series (sorted by date) is then displayed at the top of each of them.
Current series are: `Transformation journey`, `Ownership cartography`, `Parametrics in Famix`, `Famix Tools`, `Moose and Tree-sitter`, `FAST-Python analyses`.

### Draft

If you want to be sure the blogpost in not directly published, do not forget to add the frontmatter

```yaml
---
# Exclude this page from production builds
draft: true
---
```

## Authors

All the people of the project are described in a single file: [`src/content/docs/about/authors.json`](src/content/docs/about/authors.json).
It is used by the [team page](src/components/Team.astro), the [publications](src/components/Publications.astro) (to link the team members), and the blog posts authors (through [`src/utils/authors.ts`](src/utils/authors.ts)).

To add yourself, add an entry to `authors.json`:

```json
"BenoitVerhaeghe": {
    "name": "Benoit Verhaeghe",
    "role": "Cookie Lover",
    "bio": "R&D engineer at <a href='https://berger-levrault.com/'>Berger-Levrault</a>",
    "image": "https://avatars.githubusercontent.com/u/6225039",
    "github": "https://github.com/badetitou",
    "linkedin": "https://www.linkedin.com/in/benoitverhaeghe/",
    "orcid": "https://orcid.org/0000-0002-4588-2698",
    "personnal-website": "https://badetitou.fr"
}
```

- The **key** (`BenoitVerhaeghe`) is the identifier used in the `authors` field of the blog posts. Use only ASCII letters (no accent, no space).
- `name`, `role` and `image` are required. `role` is also the title displayed on the blog posts.
- `bio` is optional and may contain HTML links.
- `github`, `linkedin`, `twitter`, `orcid`, `personnal-website` are optional. The first available among `personnal-website`, `orcid`, `github` and `linkedin` is used as profile link.
- `gravatar-hash` is optional: if set, the Gravatar is used instead of `image`.
- The `name` must match the way you are named in [`bibliography.json`](src/content/docs/research/bibliography.json) to be linked in the publications.

## Using Latex

To use Latex in your blogpost, you need to:

- Create a file in `mdx` format, for example `src/content/docs/blog/2026-08-11-building-an-ownership-map-of-a-system.mdx`
- add `import 'katex/dist/katex.min.css';`

Then, inline math can be written as `$x^2$` and block math can be written as 

$$
x^2
$$
