# OG Images & Banner Images

When images are stored in this directory, they are automatically used as Open Graph (social media preview) Images and Cover/Banner Images for each blog post.

Images stored here must follow the naming convention `<post-slug>.webp` and must always be .webp files, e.g. `2023-11-21-coverlettergpt.webp`.

This is because OG Image URLs and Banner Images are automatically generated for each blog post based on the logic in the custom Title and Head components, e.g. `src/components/HeadWithOGImage.astro`:

```tsx
const ogImageUrl = new URL(
  `/banner-images/${Astro.props.id.replace(/blog\//, '').replace(/\.\w+$/, '.webp')}`,
  Astro.site,
)
```