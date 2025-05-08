// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import starlightBlog from 'starlight-blog'
import starlightSidebarTopics from 'starlight-sidebar-topics';


import tailwindcss from "@tailwindcss/vite";


// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'Modular Moose',
			customCss: [
				// Path to your Tailwind base styles:
				'./src/styles/global.css',
			],
			logo: {
				src: './src/assets/moose-logo.svg',
			},
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/moosetechnology' },
			{ icon: 'discord', label: 'Discord', href: 'https://discord.gg/QewZMZa' }
			],
			plugins: [
				starlightBlog({
					authors: {
						NicolasAnquetil: {
							name: 'Nicolas Anquetil',
							title: 'Moose expert',
							picture: 'https://avatars.githubusercontent.com/u/14889146?s=200', // Images in the `public` directory are supported.
							url: 'https://orcid.org/0000-0003-1486-8399',
						},
						BenoitVerhaeghe: {
							name: 'Benoit Verhaeghe',
							title: 'Moose expert',
							picture: 'https://avatars.githubusercontent.com/u/6225039?s=200', // Images in the `public` directory are supported.
							url: 'https://badetitou.fr',
						},
						AnneEtien: {
							name: "Anne Etien",
							title: "Moose expert",
							picture: "https://avatars.githubusercontent.com/u/17824579",
							url: "https://www.cristal.univ-lille.fr/profil/etien/"
						},
						VincentAranega: {
							name: "Vincent Aranega",
							title: "MDE and beers lover",
							url: "https://orcid.org/0000-0003-4465-1289",
							picture: "https://avatars.githubusercontent.com/u/2317394"
						},
						PascalZaragoza: {
							name: "Pascal Zaragoza",
							title: "MDE and AI",
							url: "https://orcid.org/0000-0002-5033-6392",
							picture: "https://avatars.githubusercontent.com/u/8062930"
						},
						ClotildeToullec: {
							name: "Clotilde Toullec",
							title: "Research engineer",
							url: "https://www.linkedin.com/in/clotilde-toullec-3aa685105/",
							picture: "https://avatars.githubusercontent.com/u/39184695"
						},
						ChristopherFuhrman: {
							name: "Christopher Fuhrman",
							title: "Moose herder",
							url: "https://fuhrmanator.github.io/",
							picture: "https://avatars.githubusercontent.com/u/7606540"
						},
						SebastianJordan: {
							name: "Sebastian Jordan",
							title: "Moose developer",
							picture: "https://avatars.githubusercontent.com/u/33934979",
							url: "https://linkedin.com/in/jordanmontt",
						},
						ThéoLanord: {
							name: "Théo Lanord",
							title: "Trainee / Apprentice engineer",
							url: "https://www.linkedin.com/in/théo-lanord/",
							picture: "https://avatars.githubusercontent.com/u/58632394?v=4"
						},
						RédaIdtaleb: {
							name: "Réda Id-taleb",
							title: "Working on LabelContractor and color palette project for my internship",
							url: "https://www.linkedin.com/in/r%C3%A9daid-taleb/",
							picture: "https://avatars.githubusercontent.com/u/82438570"
						},
						AhmedZakiBENNECER: {
							name: "Ahmed Zaki BENNECER",
							title: "Working on data flow analysis project for my internship",
							url: "https://ahmed-zaki-bennecer.web.app/",
							picture: "https://avatars.githubusercontent.com/u/45533819"
						},
						RomainDegrave: {
							name: "Romain Degrave",
							title: "Master degree intern in software analysis",
							url: "https://github.com/RomainDeg",
							picture: "https://avatars.githubusercontent.com/u/98580879"
						},
						PatriciaTOTOUM: {
							name: "Patricia TOTOUM",
							title: "Intern working on Famix-Aggregator",
							url: "https://www.linkedin.com/in/patricia-totoum-988179255/",
							picture: "https://zupimages.net/up/23/31/lxci.jpeg"
						},
						ThomasWattebled: {
							name: "Thomas Wattebled",
							title: "working on parametric types for my intership",
							url: "https://github.com/thomasWattebled",
							picture: "https://avatars.githubusercontent.com/u/124277643"
						},
					},
				}),
				starlightSidebarTopics(
					[
						{
							label: 'Documentation',
							link: '/beginners/install-moose/',
							items: [
								{
									label: 'Beginners',
									// Autogenerate a group of links for the 'constellations' directory.
									autogenerate: { directory: 'beginners' },
								},
								{
									label: 'Users',
									// Autogenerate a group of links for the 'constellations' directory.
									autogenerate: { directory: 'users' },
								}, {
									label: 'Developers',
									// Autogenerate a group of links for the 'constellations' directory.
									autogenerate: { directory: 'developers' },
								},
							],
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