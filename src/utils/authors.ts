import authors from '../content/docs/about/authors.json';
import { avatarUrl } from './avatar';

export type Author = (typeof authors)[keyof typeof authors] & {
	bio?: string;
	github?: string;
	linkedin?: string;
	twitter?: string;
	orcid?: string;
	'personnal-website'?: string;
	'gravatar-hash'?: string;
};

export const profileLink = (author: Author) =>
	author['personnal-website'] ?? author.orcid ?? author.github ?? author.linkedin;

// Single source of truth for the team page, the publications and the blog post authors
export const blogAuthors = Object.fromEntries(
	Object.entries(authors as Record<string, Author>).map(([id, author]) => [
		id,
		{ name: author.name, title: author.role, picture: avatarUrl(author, 200), url: profileLink(author) },
	])
);
