type AuthorWithAvatar = { image: string; 'gravatar-hash'?: string };

// Use the author's Gravatar when a hash is provided, falling back to their regular image
export const avatarUrl = (author: AuthorWithAvatar, size = 192) => {
	const hash = author['gravatar-hash'];
	if (!hash) return author.image;
	return `https://www.gravatar.com/avatar/${hash}?s=${size}&d=${encodeURIComponent(author.image)}`;
};
