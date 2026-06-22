import starlight from "@astrojs/starlight";
import { defineConfig } from "astro/config";
import starlightBlog from "starlight-blog";

import tailwind from "@astrojs/tailwind";

// https://astro.build/config
export default defineConfig({
  site: "https://docs.opensaas.sh",
  trailingSlash: "always",
  redirects: {
    "/guides/seo/": "/guides/seo-performance/",
  },
  integrations: [
    starlight({
      title: "OpenSaaS.sh",
      description:
        "Open SaaS is a free, open-source, full-stack SaaS starter kit for React + NodeJS.",
      customCss: ["./src/styles/tailwind.css"],
      logo: {
        src: "/src/assets/logo.webp",
        alt: "Open SaaS",
      },
      head: [
        {
          tag: "meta",
          attrs: {
            name: "google-site-verification",
            content: "0AMsAfi62u_qg8v_h_QIFmLvxn8k24WZaCq5q5Sfr68",
          },
        },
        {
          tag: "script",
          attrs: {
            defer: true,
            "data-domain": "docs.opensaas.sh",
            "data-api": "https://opensaas.sh/wasparadocs/wasp/event",
            src: "https://opensaas.sh/wasparadocs/wasp/script.js",
          },
        },
      ],
      editLink: {
        baseUrl:
          "https://github.com/wasp-lang/open-saas/edit/main/opensaas-sh/blog",
      },
      components: {
        SiteTitle: "./src/components/MyHeader.astro",
        // We customized ThemeSelect to include a "Copy URL for LLMs" button
        ThemeSelect: "./src/components/MyRightNavBarItems.astro",
        Head: "./src/components/HeadWithOGImage.astro",
        PageTitle: "./src/components/TitleWithBannerImage.astro",
        PageFrame: "./src/components/PageFrameWithCookieConsent.astro",
      },
      social: {
        github: "https://github.com/wasp-lang/open-saas",
        twitter: "https://twitter.com/wasplang",
        discord: "https://discord.gg/aCamt5wCpS",
      },
      sidebar: [
        {
          label: "Start Here",
          items: [
            { label: "Introduction", link: "/" },
            { label: "Getting Started", link: "/start/getting-started/" },
            { label: "Guided Tour", link: "/start/guided-tour/" },
          ],
        },
        {
          label: "Guides",
          items: [
            { label: "Analytics", link: "/guides/analytics/" },
            { label: "Authentication", link: "/guides/authentication/" },
            { label: "Authorization", link: "/guides/authorization/" },
            { label: "Cookie Consent Modal", link: "/guides/cookie-consent/" },
            {
              label: "Payment Integrations",
              items: [
                { label: "Overview", link: "/guides/payment-integrations/" },
                {
                  label: "Stripe",
                  link: "/guides/payment-integrations/stripe/",
                },
                {
                  label: "Lemon Squeezy",
                  link: "/guides/payment-integrations/lemon-squeezy/",
                },
                { label: "Polar", link: "/guides/payment-integrations/polar/" },
              ],
            },
            { label: "Deploying", link: "/guides/deploying/" },
            { label: "SEO & Performance", link: "/guides/seo-performance/" },
            { label: "Email Sending", link: "/guides/email-sending/" },
            { label: "File Uploading", link: "/guides/file-uploading/" },
            { label: "Tests", link: "/guides/tests/" },
            {
              label: "How (Not) to Update Your Open SaaS App",
              link: "/guides/updating-opensaas/",
            },
            { label: "Using AI Coding Tools", link: "/guides/vibe-coding/" },
          ],
        },
        {
          label: "General",
          autogenerate: { directory: "/general/" },
        },
      ],
      plugins: [
        starlightBlog({
          title: "Blog",
          customCss: ["./src/styles/tailwind.css"],
          authors: {
            vince: {
              name: "Vince",
              title: "Dev Rel @ Wasp",
              picture: "https://github.com/vincanger.png",
              url: "https://wasp.sh",
            },
            matija: {
              name: "Matija",
              title: "CEO @ Wasp",
              picture: "https://github.com/matijasos.png",
              url: "https://wasp.sh",
            },
            milica: {
              name: "Milica",
              title: "Growth @ Wasp",
              picture: "https://github.com/mmaksimovic.png",
              url: "https://wasp.sh",
            },
            martin: {
              name: "Martin",
              title: "CTO @ Wasp",
              picture: "https://github.com/martinsos.png",
              url: "https://wasp.sh",
            },
          },
        }),
      ],
    }),
    tailwind({ applyBaseStyles: false }),
  ],
});
