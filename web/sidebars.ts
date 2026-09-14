import type { SidebarsConfig } from "@docusaurus/plugin-content-docs";
import type {
  SidebarItemConfig,
  SidebarItemLink,
} from "@docusaurus/plugin-content-docs/src/sidebars/types.js";
import typedocSidebar from "./docs/api/typedoc-sidebar";

const sidebars: SidebarsConfig = {
  docs: [
    {
      type: "category",
      label: "Getting started",
      collapsed: false,
      collapsible: true,
      items: [
        "getting-started/introduction",
        "getting-started/quick-start",
        "getting-started/starter-templates",
        "getting-started/agent-integration",
      ],
    },
    {
      type: "category",
      label: "Tutorial",
      collapsed: true,
      collapsible: true,
      link: { type: "doc", id: "tutorial/create" },
      items: [
        "tutorial/project-structure",
        "tutorial/pages",
        "tutorial/entities",
        "tutorial/queries",
        "tutorial/actions",
        "tutorial/auth",
      ],
    },
    {
      type: "category",
      label: "Features",
      collapsed: false,
      collapsible: true,
      items: [
        "features/spec",
        // TODO: Pages docs go here: https://github.com/wasp-lang/wasp/issues/2072
        "features/routes",
        {
          type: "category",
          label: "Data",
          collapsed: true,
          collapsible: true,
          items: [
            "features/data/entities",
            {
              type: "category",
              label: "Operations",
              collapsed: true,
              link: { type: "doc", id: "features/data/operations/overview" },
              items: [
                "features/data/operations/queries",
                "features/data/operations/actions",
              ],
            },
            "features/data/crud",
            "features/data/databases",
            "features/data/prisma-file",
          ],
        },
        {
          type: "category",
          label: "Auth",
          collapsed: true,
          collapsible: true,
          link: { type: "doc", id: "features/auth/overview" },
          items: [
            "features/auth/ui",
            {
              type: "category",
              label: "Username and password",
              collapsed: true,
              link: {
                type: "doc",
                id: "features/auth/username-and-pass/overview",
              },
              items: ["features/auth/username-and-pass/create-your-own-ui"],
            },
            {
              type: "category",
              label: "Email",
              collapsed: true,
              link: { type: "doc", id: "features/auth/email/overview" },
              items: ["features/auth/email/create-your-own-ui"],
            },
            {
              type: "category",
              label: "Social auth",
              collapsed: true,
              link: { type: "doc", id: "features/auth/social-auth/overview" },
              items: [
                "features/auth/social-auth/github",
                "features/auth/social-auth/google",
                "features/auth/social-auth/keycloak",
                "features/auth/social-auth/slack",
                "features/auth/social-auth/discord",
                "features/auth/social-auth/microsoft",
                "features/auth/social-auth/create-your-own-ui",
              ],
            },
            "features/auth/entities",
            "features/auth/hooks",
            {
              type: "category",
              label: "Advanced",
              collapsed: true,
              items: ["features/auth/advanced/custom-auth-actions"],
            },
          ],
        },
        "features/email",
        "features/jobs",
        "features/websockets",
        "features/apis",
      ],
    },
    {
      type: "category",
      label: "Advanced",
      collapsed: false,
      collapsible: true,
      items: [
        "advanced/dependencies",
        "advanced/links",
        "advanced/git-worktrees",
        "advanced/env-vars",
        "advanced/testing",
        "advanced/accessing-app-config",
        "advanced/prerendering",
        "advanced/seo",
        {
          type: "category",
          label: "Client customization",
          collapsed: true,
          collapsible: true,
          items: [
            "advanced/client-customization/customizing-app",
            "advanced/client-customization/client-config",
            "advanced/client-customization/static-assets",
            "advanced/client-customization/custom-vite-config",
          ],
        },
        {
          type: "category",
          label: "Server customization",
          collapsed: true,
          collapsible: true,
          items: [
            "advanced/server-customization/server-config",
            "advanced/server-customization/middleware",
          ],
        },
        "advanced/cli",
      ],
    },
    {
      type: "category",
      label: "Deployment",
      collapsed: false,
      collapsible: true,
      link: { type: "doc", id: "deployment/overview" },
      items: [
        "deployment/env-vars",
        "deployment/database",
        "deployment/local-testing",
        {
          type: "category",
          label: "Methods",
          collapsed: false,
          collapsible: true,
          link: { type: "doc", id: "deployment/methods/overview" },
          items: [
            {
              type: "category",
              label: "Wasp Deploy",
              collapsed: false,
              collapsible: true,
              link: {
                type: "doc",
                id: "deployment/methods/wasp-deploy/overview",
              },
              items: [
                "deployment/methods/wasp-deploy/fly",
                "deployment/methods/wasp-deploy/railway",
                "deployment/methods/wasp-deploy/cd",
              ],
            },
            "deployment/methods/cloud-providers",
            "deployment/methods/self-hosted",
          ],
        },
        "deployment/ci-cd",
        "deployment/extras",
      ],
    },
    {
      type: "category",
      label: "Migration guides",
      collapsed: true,
      collapsible: true,
      link: { type: "doc", id: "migration-guide" },
      items: [
        ...generateMigrationGuideLinks("0.11", [
          "0.12",
          "0.13",
          "0.14",
          "0.15",
          "0.16",
          "0.17",
          "0.18",
          "0.19",
          "0.20",
          "0.21",
          "0.22",
          "0.23",
          "0.24",
          "0.25",
        ]),
      ],
    },
    {
      type: "link",
      label: "Examples",
      href: "https://github.com/wasp-lang/wasp/tree/release/examples",
    },
    "telemetry",
    "contributing",
    "vision",
    {
      type: "link",
      label: "Roadmap",
      href: "https://github.com/orgs/wasp-lang/projects/5",
    },
    "contact",
  ],
  guides: [
    {
      type: "autogenerated",
      dirName: "guides",
    },
  ],
  api: [
    {
      type: "category",
      label: "packages",
      collapsed: false,
      collapsible: false,
      items: typedocSidebar.items,
    },
  ],
};

export default sidebars;

function generateMigrationGuideLinks(
  earliestUndocumentedVersion: string,
  docsVersions: string[],
): SidebarItemConfig[] {
  return docsVersions
    .map((currentVersion, index, arr): SidebarItemLink => {
      const prevVersion =
        index == 0 ? earliestUndocumentedVersion : arr[index - 1];

      return {
        type: "link",
        label: `From ${prevVersion} to ${currentVersion}`,
        href: `/docs/${currentVersion}/migration-guide`,
      };
    })
    .reverse();
}
