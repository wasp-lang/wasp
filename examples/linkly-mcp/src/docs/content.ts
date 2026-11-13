// Linkly's docs, as data. `{{serverUrl}}` and `{{appUrl}}` are filled in when
// a doc is read, so examples show the real URLs of this deployment.
export type Doc = { slug: string; title: string; tags: string[]; body: string };

export const DOCS: Doc[] = [
  {
    slug: "getting-started",
    title: "Getting started",
    tags: ["overview", "basics"],
    body: `Linkly turns long URLs into short links and counts how often they're clicked.

1. **Create a link** on the Links page: paste the destination URL, optionally pick a slug.
2. **Share the short URL**: \`{{serverUrl}}/r/<slug>\`. Each visit redirects to the destination and is counted.
3. **Watch the clicks** on the link's Stats tab or on Top links.

Everything in the dashboard is also available to AI assistants through Linkly's [MCP server](/docs/connecting-ai-clients).`,
  },
  {
    slug: "links-and-slugs",
    title: "Links and slugs",
    tags: ["links", "slugs", "short url"],
    body: `A **link** has a destination URL, a **slug**, an optional title, and up to 10 tags.

The slug is the short code in the short URL: \`{{serverUrl}}/r/<slug>\`.

- Slugs are 3–40 characters: lowercase letters, numbers, and hyphens.
- Slugs are unique across all of Linkly. If one is taken you'll get a conflict error: pick another.
- Leave the slug empty to get a random 7-character one.
- Changing a slug changes the short URL; the old one stops working.

**Archiving** a link stops its short URL from redirecting (visitors see "not found") and hides it from the default list, without deleting its history. Unarchive it to bring it back.

Anywhere Linkly asks for a link, you can pass its ID or its slug.`,
  },
  {
    slug: "tags",
    title: "Tags",
    tags: ["tags", "organize", "filter"],
    body: `Tags group links, for example by campaign (\`summer-sale\`) or channel (\`newsletter\`).

- Up to 10 tags per link, each up to 30 characters.
- Tags are stored in lowercase, so \`Promo\` and \`promo\` are the same tag.
- Filter the Links page (or \`list_links\`) by one tag.
- Updating a link's tags replaces the whole list.`,
  },
  {
    slug: "how-clicks-are-counted",
    title: "How clicks are counted",
    tags: ["clicks", "analytics", "counting", "referrer", "privacy"],
    body: `A click is counted every time someone opens a short URL and is redirected.

How clicks are counted:

- **Every redirect counts**, including repeat visits from the same person. Linkly doesn't deduplicate visitors.
- **Archived or deleted links** don't redirect, so their visits aren't counted.
- **Referrer**: only the hostname of the referring page is stored (for example \`news.ycombinator.com\`), never the full URL. Visits without a referrer are grouped as \`direct\`.
- **User agent**: stored, truncated to 200 characters. Linkly doesn't store IP addresses or locations.
- Clicks are recorded right after the redirect is sent, so a click shows up in stats within a moment.

Deleting a link deletes its clicks too.`,
  },
  {
    slug: "analytics-ranges",
    title: "Analytics time ranges",
    tags: ["analytics", "ranges", "stats", "charts"],
    body: `Stats and top links take a **range**:

| Range | Covers |
|---|---|
| \`24h\` | The last 24 hours, exactly |
| \`7d\` | Today and the 6 previous days (UTC) |
| \`30d\` | Today and the 29 previous days (UTC) |
| \`90d\` | Today and the 89 previous days (UTC) |

Stats can be grouped **by day** (one entry per UTC day, including days with zero clicks, so charts have no gaps) or **by referrer** (one entry per referring website, most clicks first).

Every stats result includes a \`dashboardUrl\` that opens the same chart in Linkly.`,
  },
  {
    slug: "connecting-ai-clients",
    title: "Connecting AI assistants (MCP)",
    tags: ["mcp", "ai", "claude", "oauth", "connect", "assistant"],
    body: `Linkly has an MCP server, so AI assistants can manage your links for you.

**Connect:** add this URL as a custom connector (or MCP server) in your assistant:

\`\`\`
{{serverUrl}}/mcp
\`\`\`

The assistant opens Linkly's consent page. Check which app is asking (Linkly shows its address), pick what it [may do](/docs/permissions) (for example, view your links but not change them), choose whether deletions need your approval, and click **Allow**. There's no API key to copy and no app registration: Linkly accepts any app that publishes a client metadata document.

**Manage** connected apps on [Connections]({{appUrl}}/settings/connections): change permissions, toggle approvals, or disconnect. Changes apply on the app's next request.`,
  },
  {
    slug: "permissions",
    title: "Permissions",
    tags: ["permissions", "scopes", "security", "oauth"],
    body: `Permissions are **scopes**:

| Scope | Allows |
|---|---|
| \`links:read\` | View your links |
| \`links:write\` | Create, edit, and delete your links |
| \`analytics:read\` | View click analytics |

Your dashboard session has every scope. On the consent screen, each scope is its own checkbox, so you can let an AI app view your links without letting it change them. Ticking a write scope also ticks its read scope, since changing a link also shows it. The scopes the app asked for are ticked to start with. On Connections you can remove scopes later or restore them, up to what the app asked for.

A connected app can only use the tools you allowed. If it tries another, Linkly asks you to allow that scope on the consent screen, or you can restore it on Connections. A connected app can never approve other apps or change your connections; only you can, from the dashboard.

Some tools are always available because they don't touch your data: \`whoami\`, \`search_docs\`, \`read_doc\`, and \`get_dashboard_url\`.`,
  },
  {
    slug: "deleting-links",
    title: "Deleting links",
    tags: ["delete", "confirmation", "safety", "archive"],
    body: `Deleting a link removes it and its click history permanently.

- **In the dashboard**, you confirm in a dialog.
- **Connected apps** follow your setting for that connection. With **Ask me before it does anything that can't be undone** on (the default), nothing is deleted until you approve it in Linkly. Apps that can open web pages open the approval page for you; others share a link to it. The page shows which link would be deleted and how many clicks it has. The request expires after 15 minutes.

If you might want the link back, **archive** it instead: it stops redirecting but keeps its history.`,
  },
];
