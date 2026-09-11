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
        "introduction/introduction",
        "introduction/quick-start",
        "project/starter-templates",
        "wasp-ai/coding-agent-plugin",
      ],
    },
    {
      type: "category",
      label: "Tutorial",
      collapsed: true,
      collapsible: true,
      link: { type: "doc", id: "tutorial/create" },
      items: [
        "tutorial/create",
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
        "general/spec",
        // TODO: Pages docs go here: https://github.com/wasp-lang/wasp/issues/2072
        "advanced/routing",
        {
          type: "category",
          label: "Data",
          collapsed: true,
          collapsible: true,
          items: [
            "data-model/entities",
            {
              type: "category",
              label: "Operations",
              collapsed: true,
              link: { type: "doc", id: "data-model/operations/overview" },
              items: [
                "data-model/operations/queries",
                "data-model/operations/actions",
              ],
            },
            "data-model/crud",
            "data-model/databases",
            "data-model/prisma-file",
          ],
        },
        {
          type: "category",
          label: "Auth",
          collapsed: true,
          collapsible: true,
          link: { type: "doc", id: "auth/overview" },
          items: [
            "auth/ui",
            {
              type: "category",
              label: "Username and password",
              collapsed: true,
              link: { type: "doc", id: "auth/username-and-pass" },
              items: ["auth/username-and-pass/create-your-own-ui"],
            },
            {
              type: "category",
              label: "Email",
              collapsed: true,
              link: { type: "doc", id: "auth/email" },
              items: ["auth/email/create-your-own-ui"],
            },
            {
              type: "category",
              label: "Social auth",
              collapsed: true,
              link: { type: "doc", id: "auth/social-auth/overview" },
              items: [
                "auth/social-auth/github",
                "auth/social-auth/google",
                "auth/social-auth/keycloak",
                "auth/social-auth/slack",
                "auth/social-auth/discord",
                "auth/social-auth/microsoft",
                "auth/social-auth/create-your-own-ui",
              ],
            },
            "auth/entities/entities",
            "auth/auth-hooks",
            {
              type: "category",
              label: "Advanced",
              collapsed: true,
              items: ["auth/advanced/custom-auth-actions"],
            },
          ],
        },
        "advanced/email",
        "advanced/jobs",
        "advanced/web-sockets",
        "advanced/apis",
      ],
    },
    {
      type: "category",
      label: "Advanced",
      collapsed: false,
      collapsible: true,
      items: [
        "project/dependencies",
        "advanced/links",
        "wasp-ai/git-worktrees",
        "project/env-vars",
        "project/testing",
        "advanced/accessing-app-config",
        "advanced/prerendering",
        "advanced/seo",
        {
          type: "category",
          label: "Client customization",
          collapsed: true,
          collapsible: true,
          items: [
            "project/customizing-app",
            "project/client-config",
            "project/static-assets",
            "project/custom-vite-config",
          ],
        },
        {
          type: "category",
          label: "Server customization",
          collapsed: true,
          collapsible: true,
          items: ["project/server-config", "advanced/middleware-config"],
        },
        "general/cli",
      ],
    },
    {
      type: "category",
      label: "Deployment",
      collapsed: true,
      collapsible: true,
      link: { type: "doc", id: "deployment/intro" },
      items: [
        "deployment/env-vars",
        "deployment/database",
        "deployment/local-testing",
        {
          type: "category",
          label: "Methods",
          collapsed: false,
          collapsible: true,
          link: { type: "doc", id: "deployment/deployment-methods/overview" },
          items: [
            {
              type: "category",
              label: "Wasp Deploy",
              collapsed: false,
              collapsible: true,
              link: {
                type: "doc",
                id: "deployment/deployment-methods/wasp-deploy/overview",
              },
              items: [
                "deployment/deployment-methods/wasp-deploy/fly",
                "deployment/deployment-methods/wasp-deploy/railway",
                "deployment/deployment-methods/wasp-deploy/ci-cd",
              ],
            },
            "deployment/deployment-methods/cloud-providers",
            "deployment/deployment-methods/self-hosted",
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
        { type: "doc", id: "migration-guide" },
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
