import type { LoadedVersion } from "@docusaurus/plugin-content-docs";

export type Sidebars = { [sidebarName: string]: ResolvedSidebarItem[] };

export type ResolvedSidebarItem = ResolvedSidebarLink | ResolvedSidebarCategory;

export interface ResolvedSidebarLink {
  type: "link";
  href: string;
  label: string;
  docId?: string;
}

export interface ResolvedSidebarCategory {
  type: "category";
  label: string;
  items: ResolvedSidebarItem[];
}

type LoadedSidebarItem = LoadedVersion["sidebars"][string][number];
type LoadedDoc = LoadedVersion["docs"][number];

/**
 * Resolves the docs plugin's in-memory sidebars into link/category items with
 * concrete hrefs and labels, indexed by Wasp version.
 *
 * Docusaurus keeps sidebar doc entries as bare `{ type: "doc", id }`, so we
 * join each against the version's docs to recover its permalink and label,
 * reproducing the resolved shape it would otherwise write to the route props.
 */
export function resolveSidebarsForDocsLoadedVersions(
  loadedVersion: LoadedVersion,
): Sidebars {
  const docsById = new Map(loadedVersion.docs.map((doc) => [doc.id, doc]));
  const resolvedSidebars: Sidebars = {};
  for (const [sidebarName, items] of Object.entries(loadedVersion.sidebars)) {
    resolvedSidebars[sidebarName] = resolveSidebarItems(items, docsById);
  }
  return resolvedSidebars;
}

function resolveSidebarItems(
  items: LoadedSidebarItem[],
  docsById: Map<string, LoadedDoc>,
): ResolvedSidebarItem[] {
  const resolvedItems: ResolvedSidebarItem[] = [];
  for (const item of items) {
    const resolvedItem = resolveSidebarItem(item, docsById);
    if (resolvedItem) {
      resolvedItems.push(resolvedItem);
    }
  }
  return resolvedItems;
}

function resolveSidebarItem(
  item: LoadedSidebarItem,
  docsById: Map<string, LoadedDoc>,
): ResolvedSidebarItem | null {
  if (item.type === "category") {
    return {
      type: "category",
      label: item.label,
      items: resolveSidebarItems(item.items, docsById),
    };
  }
  if (item.type === "link") {
    return { type: "link", href: item.href, label: item.label };
  }
  if (item.type === "doc" || item.type === "ref") {
    const doc = docsById.get(item.id);
    if (!doc) {
      return null;
    }
    return {
      type: "link",
      href: doc.permalink,
      label: item.label ?? doc.frontMatter.sidebar_label ?? doc.title,
      docId: doc.id,
    };
  }
  // `html` and any future item types have no doc route, so we drop them.
  return null;
}

export function isSidebarCategory(
  item: ResolvedSidebarItem,
): item is ResolvedSidebarCategory {
  return item.type === "category";
}

export function isSidebarLink(
  item: ResolvedSidebarItem,
): item is ResolvedSidebarLink {
  return item.type === "link";
}
