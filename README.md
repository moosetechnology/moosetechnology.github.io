# Modular moose website

## 🚀 Project Structure

Inside of your project, you'll see the following folders and files:

```txt
.
├── public/
├── src/
│   ├── assets/
│   ├── content/
│   │   ├── docs/
│   │   ├── blog/
│   └── content.config.ts
├── astro.config.mjs
├── package.json
└── tsconfig.json
```

Folder **Blog** is used for the blogs

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

### Adding a new docs page

When adding a new wiki page, you must link the page in the [astro.config.mjs](astro.config.mjs) under the `starlightSidebarTopics` item.
It allows one to organize the page.

> There are automatic way to import all pages, but they sort files by name, and sometimes we do not want that.

## Authoring a blog post

1. Create a new file under `src/content/blog`
2. add the yaml

```yaml
---
authors:
- BenoitVerhaeghe
title: "Coasters collection"
subtitle: "Or how to create a minimal model in Moose 8"
date:  2021-02-15
lastUpdated:  2021-03-04
tags:
- meta-model
---
```

For authors, use the key identified in `astro.config.mjs`

### Draft

If you want to be sure the blogpost in not directly published, do not forget to add the frontmatter

```yaml
---
# Exclude this page from production builds
draft: true
---
```

## Add authors

If you need to be added as author.

1. Edit the file `astro.config.mjs` if the appropriate data.
2; Edit the file `src/content/docs/about/authors.json` with the appropriate data.

## Using Latex

To use Latex in your blogpost, you need to:

- Create a file in `mdx` format, for example `src/content/docs/blog/2026-08-11-building-an-ownership-map-of-a-system.mdx`
- add `import 'katex/dist/katex.min.css';`

Then, inline math can be written as `$x^2$` and block math can be written as 

$$
x^2
$$
