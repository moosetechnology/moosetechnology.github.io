import { defineCollection } from 'astro:content';
import { z } from 'astro/zod';
import { docsLoader } from '@astrojs/starlight/loaders';
import { docsSchema } from '@astrojs/starlight/schema';
import { blogSchema } from 'starlight-blog/schema'

export const collections = {
	docs: defineCollection({
		loader: docsLoader(), schema: docsSchema(
			{
				extend: (context) => blogSchema(context).extend({
					// Name of the series the blog post belongs to (see src/components/BlogSeries.astro)
					series: z.string().optional(),
				})
			}
		)
	}),
};
