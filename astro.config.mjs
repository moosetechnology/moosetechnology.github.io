// @ts-check
import { readFileSync } from 'node:fs';
import { defineConfig, sharpImageService } from 'astro/config';
import starlight from '@astrojs/starlight';
import starlightCatppuccin from '@catppuccin/starlight';
import starlightBlog from 'starlight-blog'
import starlightLlmsTxt from 'starlight-llms-txt';
import { blogAuthors } from './src/utils/authors';
import starlightSidebarTopics from 'starlight-sidebar-topics';
import tailwindcss from "@tailwindcss/vite";
import remarkSimplePlantumlPlugin from "@akebifiky/remark-simple-plantuml"
import rehypeMermaid from 'rehype-mermaid';
import remarkGemoji from 'remark-gemoji';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';
import { unified } from '@astrojs/markdown-remark';

const mermaidThemeCSS = readFileSync(
	new URL('./src/styles/mermaid-theme.css', import.meta.url),
	'utf8'
);

// https://astro.build/config
export default defineConfig({
	markdown: {
		// Applied to .md and .mdx files
		processor: unified({
			remarkPlugins: [
				[remarkSimplePlantumlPlugin, { baseUrl: "https://www.plantuml.com/plantuml/svg" }],
				remarkGemoji,
				remarkMath
			],
			rehypePlugins: [
				[rehypeMermaid, {
					mermaidConfig: {
						theme: 'base',
						themeCSS: mermaidThemeCSS
					}
				}],
				rehypeKatex
			]
		})
	},
	image: {
		service: sharpImageService({ limitInputPixels: false }),
	},
	site: 'https://modularmoose.org',
	base: '',
	redirects: {
		'/users/git-project-health/getting-started-with-gitproject-health': '/users/git-project-health/getting-started',
		'/users/git-project-health/local-gitproject-health': '/users/git-project-health/importers#local-importer',
		'/users/git-project-health/mixed-gitproject-health': '/users/git-project-health/importers#mixed-importer',
	},
	integrations: [
		starlight({
			title: 'Modular Moose',
			components: {
				// Wraps the starlight-blog override (expected warning from starlight-blog at startup)
				MarkdownContent: './src/components/MarkdownContent.astro',
			},
			expressiveCode: {
				shiki: {
					langAlias: {
						st: 'smalltalk',
						ston: 'smalltalk'
					 },
				},
			},
			customCss: [
				// Path to your Tailwind base styles:
				'./src/styles/global.css',
			],
			editLink: {
				baseUrl: 'https://github.com/moosetechnology/moosetechnology.github.io/tree/main/'
			},

			logo: {
				src: './src/assets/moose-logo.svg',
			},
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/moosetechnology' },
			{ icon: 'discord', label: 'Discord', href: 'https://discord.gg/QewZMZa' }
			],
			plugins: [
				starlightLlmsTxt(),
				starlightCatppuccin({
					dark: { flavor: 'mocha', accent: 'blue' },
					light: { flavor: 'latte', accent: 'blue' },
				}),
				starlightBlog({ authors: blogAuthors }),
				starlightSidebarTopics(
					[
						{
							label: 'Documentation',
							link: '/beginners/what-is-moose/',
							items: [
								{
									label: 'Getting started',
									items: [
										{
											label: 'What is Moose?',
											link: '/beginners/what-is-moose/',
										},
										{
											label: 'Install Moose',
											link: '/beginners/install-moose/',
										},
										{
											label: 'Your first analysis',
											link: '/beginners/moose-in-action/',
										},
										{
											label: 'Analyze your own project',
											link: '/beginners/analyze-your-project/',
										},
										{
											label: 'Next steps',
											link: '/beginners/next-steps/',
										},
										{
											label: 'Demo video',
											link: 'https://www.youtube.com/watch?v=2ILbR5ZrSm8',
											badge: { text: 'Outside', variant: 'tip' }
										}
									],
								},
								{
									label: 'Users',
									items: [
										{
											label: 'Famix Java',
											items: [
												{
													label: 'Famix Java',
													link: '/users/famix-java/famix-java',
												},
												{
													label: 'Generate Java Code',
													link: '/users/famix-java/generate-java-code',
												},
												{
													label: 'Demo Analyse Java Project',
													link: 'https://fuhrmanator.github.io/posts/analyzing-java-with-moose/index.html',
													badge: { text: 'Outside', variant: 'tip' }
												},
											]
										},
										{

											label: 'Famix TypeScript',
											link: 'https://fuhrmanator.github.io/posts/typescript-in-moose/index.html',
											badge: { text: 'Outside', variant: 'tip' }

										},
										{
											label: 'Famix SQL',
											link: '/users/famix-sql/getting-started-with-famixsql',

										},
										{
											label: 'Mining Software Repository with GitProjectHealth',
											items: [{ autogenerate: { directory: 'users/git-project-health' } }],
										},
										{
											label: 'Moose Generic',
											items: [
												{
													label: 'Importing and exporting models',
													link: '/users/importing-and-exporting-models',
												},
												{
													label: 'Moose supported file format',
													link: '/users/file-format',
												}

											]
										},
										{
											label: 'AST',
											items: [
												{
													label: 'FAST',
													link: '/users/ast/fast',
												},
												{
													label: 'FAST Java',
													link: '/users/ast/fast-java',
												},
												{
													label: 'FAST Pharo',
													link: '/users/ast/fast-pharo',
												},
												{
													label: 'FAST Python',
													link: '/users/ast/fast-python',
												}

											]
										},
										{
											label: 'Performing queries',
											items: [
												{
													label: 'Typical Queries',
													link: '/users/typical-queries',
												},
												{
													label: 'Moose Query',
													link: '/users/moosequery',
												},
												{
													label: 'Tree Query',
													link: 'https://github.com/juliendelplanque/TreeQuery',
													badge: { text: 'Outside', variant: 'tip' }
												}

											]
										},
										{
											label: 'Moose IDE',
											items: [
												{
													label: 'Moose IDE Browsers',
													link: '/users/moose-ide/browsers',
												},
												{
													label: 'Moose Exporter',
													link: '/users/moose-ide/moose-exporter',
												},
												{
													label: 'Moose Critics',
													link: '/users/moose-ide/moose-critics',
												},
												{
													label: 'Distribution Map',
													link: '/users/moose-ide/distribution-map',
												},
												{
													label: 'Inspector',
													link: '/users/moose-ide/inspector',
												}

											]
										},
									]
								}, {
									label: 'Developers',
									// Autogenerate a group of links for the 'developers' directory.
									items: [{ autogenerate: { directory: 'developers' } }],
								},
							],
						},
						{
							label: 'Blog',
							link: '/blog/',
							items: []
						},
						{
							label: 'Research',
							link: '/research/',
							items: []
						},
						{
							label: 'About',
							link: '/about/',
							items: []
						},
					],
					{
						exclude: ['/blog', '/blog/**/*'],
					},
				)]
		}),
	],

	vite: {
		plugins: [tailwindcss()],
	},
});
