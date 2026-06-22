declare module "virtual:starlight-blog-config" {
  const StarlightBlogConfig: import("./libs/config").StarlightBlogConfig;

  export default StarlightBlogConfig;
}
declare module "virtual:starlight/user-config" {
  const Config: import("@astrojs/starlight/types").StarlightConfig;

  export default Config;
}
declare module "virtual:starlight/user-images" {
  type ImageMetadata = import("astro").ImageMetadata;
  export const logos: {
    dark?: ImageMetadata;
    light?: ImageMetadata;
  };
}
declare module "virtual:astro-config" {
  const Config: import("@astrojs/types").Config;

  export default Config;
}
