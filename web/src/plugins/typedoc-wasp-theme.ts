/**
 * TypeDoc plugin with the Wasp docs' Markdown theme, which:
 *
 * - Titles module pages with the module's full import path, e.g.
 *   `wasp/auth/providers` instead of `auth/providers`.
 * - Titles React components as `<Name>`, instead of `Name()`, and puts them in
 *   the "Components" group unless they set a `@group`. Mark a component with
 *   the `@component` modifier tag in its doc comment.
 * - Gives every heading a page-unique ID (see `addUniqueHeadingIds`).
 * - Adds a callout to declarations that stand in for the ones Wasp generates
 *   per app, and puts them in the "Placeholder Types" group unless they set a
 *   `@group`. Mark a declaration with the `@placeholder` modifier tag in its doc
 *   comment.
 * - Puts the types that `typedoc-plugin-missing-exports` documents in the
 *   "Referenced Types" group unless they set a `@group`, and adds a description
 *   to that group.
 */

import GithubSlugger from "github-slugger";
import {
  Comment,
  CommentTag,
  ContainerReflection,
  Converter,
  DeclarationReflection,
  ReflectionKind,
  type Application,
  type Reflection,
} from "typedoc";
import {
  MarkdownPageEvent,
  MarkdownTheme,
  MarkdownThemeContext,
} from "typedoc-plugin-markdown";

const COMPONENT_TAG = "@component";
const PLACEHOLDER_TAG = "@placeholder";
const GROUP_TAG = "@group";
const CATEGORY_TAG = "@category";

const COMPONENTS_GROUP = "Components";
const PLACEHOLDER_TYPES_GROUP = "Placeholder Types";
const REFERENCED_TYPES_GROUP = "Referenced Types";

const PLACEHOLDER_CALLOUT = `:::note[Placeholder]
This type isn't a real export of the SDK, so you can't import it by this name. It describes the ones Wasp generates for your app, named after what you declare in your Wasp spec.
:::`;

// `typedoc-plugin-missing-exports` documents the missing exports in a
// `RESOLVE_BEGIN` listener with this priority.
const MISSING_EXPORTS_PLUGIN_PRIORITY = 1e9;

export function load(app: Application) {
  const referencedTypes = trackReferencedTypes(app);

  // Runs before TypeDoc groups the reflections (at priority -100).
  app.converter.on(Converter.EVENT_RESOLVE_END, (context) => {
    for (const reflection of Object.values(context.project.reflections)) {
      const group = getDefaultGroup(reflection, referencedTypes);
      if (group) addDefaultGroup(reflection, group);
    }
  });

  app.renderer.defineTheme("wasp-markdown", WaspMarkdownTheme);

  app.renderer.on(MarkdownPageEvent.END, (page) => {
    if (page.contents) {
      page.contents = addUniqueHeadingIds(
        // Bundlers name React's namespace import `_$react` in the libs' built
        // types, and it shows up in the types of re-exported components.
        page.contents.replaceAll("_$react.", "React."),
      );
    }
  });
}

/** Collects the types that `typedoc-plugin-missing-exports` documents. */
function trackReferencedTypes(app: Application) {
  let isDocumentingMissingExports = false;
  const referencedTypes = new Set<Reflection>();

  app.converter.on(
    Converter.EVENT_RESOLVE_BEGIN,
    () => {
      isDocumentingMissingExports = true;
    },
    MISSING_EXPORTS_PLUGIN_PRIORITY + 1,
  );

  app.converter.on(
    Converter.EVENT_CREATE_DECLARATION,
    (_context, reflection) => {
      if (
        isDocumentingMissingExports &&
        reflection.parent?.kindOf(
          ReflectionKind.Project | ReflectionKind.Module,
        )
      ) {
        referencedTypes.add(reflection);
      }
    },
  );

  app.converter.on(
    Converter.EVENT_RESOLVE_BEGIN,
    () => {
      isDocumentingMissingExports = false;
    },
    MISSING_EXPORTS_PLUGIN_PRIORITY - 1,
  );

  return referencedTypes;
}

function getDefaultGroup(
  reflection: Reflection,
  referencedTypes: Set<Reflection>,
) {
  if (referencedTypes.has(reflection)) return REFERENCED_TYPES_GROUP;

  const comments = getComments(reflection);
  if (comments.some((comment) => comment.hasModifier(COMPONENT_TAG))) {
    return COMPONENTS_GROUP;
  }
  if (comments.some((comment) => comment.hasModifier(PLACEHOLDER_TAG))) {
    return PLACEHOLDER_TYPES_GROUP;
  }
}

function addDefaultGroup(reflection: Reflection, group: string) {
  // Modules with any category are listed by category instead of by group.
  const usesCategories =
    reflection.parent instanceof ContainerReflection &&
    (reflection.parent.children ?? [])
      .flatMap(getComments)
      .some((comment) => comment.getTag(CATEGORY_TAG));
  const tags: `@${string}`[] = usesCategories
    ? [GROUP_TAG, CATEGORY_TAG]
    : [GROUP_TAG];

  const comments = getComments(reflection);
  for (const tag of tags) {
    if (!comments.some((comment) => comment.getTag(tag))) {
      reflection.comment ??= new Comment();
      reflection.comment.blockTags.push(
        new CommentTag(tag, [{ kind: "text", text: group }]),
      );
    }
  }
}

const CODE_FENCE_PATTERN = /^\s*(```|~~~)/;
const HEADING_PATTERN = /^(#{1,6}) (.*?)(?: \{#([^}]+)\})?$/;

/**
 * With `useCustomAnchors`, TypeDoc gives explicit IDs (`{#auth-1}`) to the
 * headings of its reflections, but not to the others (e.g. "Parameters", or
 * the properties of an inlined object parameter). Docusaurus slugs those
 * without accounting for the explicit IDs, so they can end up with the same
 * ID as a reflection, and links to the reflection land on them instead.
 *
 * So we give those headings an explicit ID too, slugged like Docusaurus does
 * but skipping the IDs already in use on the page.
 */
function addUniqueHeadingIds(markdown: string) {
  const lines = markdown.split("\n");

  const headings: { index: number; match: RegExpMatchArray }[] = [];
  let inCodeBlock = false;
  lines.forEach((line, index) => {
    if (CODE_FENCE_PATTERN.test(line)) {
      inCodeBlock = !inCodeBlock;
    } else if (!inCodeBlock) {
      const match = line.match(HEADING_PATTERN);
      if (match) headings.push({ index, match });
    }
  });

  const slugger = new GithubSlugger();
  // Slugging the explicit IDs reserves them, as they are already slugs.
  for (const { match } of headings) {
    const [, , , explicitId] = match;
    if (explicitId) slugger.slug(explicitId, true);
  }

  for (const { index, match } of headings) {
    const [, hashes, text, explicitId] = match;
    if (explicitId) continue;
    const id = slugger.slug(getHeadingText(text));
    // Docusaurus uses the page's H1 as its title, so we leave it alone.
    if (hashes !== "#") lines[index] = `${lines[index]} {#${id}}`;
  }

  return lines.join("\n");
}

/** Approximates the text that Docusaurus slugs from a heading's Markdown. */
function getHeadingText(markdown: string) {
  return markdown
    .replace(/\[([^\]]*)\]\([^)]*\)/g, "$1")
    .replace(/\\(.)/g, "$1")
    .replaceAll("`", "");
}

class WaspMarkdownTheme extends MarkdownTheme {
  override getRenderContext(page: MarkdownPageEvent<Reflection>) {
    return new WaspMarkdownThemeContext(this, page, this.application.options);
  }
}

class WaspMarkdownThemeContext extends MarkdownThemeContext {
  constructor(...args: ConstructorParameters<typeof MarkdownThemeContext>) {
    super(...args);
    const { comment, memberTitle, pageTitle, parametersList } = this.partials;

    this.partials = {
      ...this.partials,

      // `typedoc-plugin-markdown` shows all modifier tags, but `@component` and
      // `@placeholder` are only for this plugin.
      comment: (model, options) => {
        const modelWithoutTags = model.clone();
        modelWithoutTags.removeModifier(COMPONENT_TAG);
        modelWithoutTags.removeModifier(PLACEHOLDER_TAG);
        const md = comment(modelWithoutTags, options);
        return model.hasModifier(PLACEHOLDER_TAG) &&
          options?.showSummary !== false
          ? `${PLACEHOLDER_CALLOUT}\n\n${md}`
          : md;
      },

      memberTitle: (model) =>
        isComponent(model)
          ? // The tag is escaped so MDX doesn't parse it as JSX.
            `\\<${model.name}\\>`
          : memberTitle(model),

      // Wraps the properties of an inlined object parameter, so we can style
      // them as nested under the parameter.
      parametersList: (model, options) =>
        model
          .map((parameter) => {
            const md = parametersList([parameter], options);
            const start = md.indexOf(
              `\n${"#".repeat(options.headingLevel + 2)} `,
            );
            return start === -1
              ? md
              : `${md.slice(0, start)}\n\n<div className="api-nested">\n${md.slice(start)}\n\n</div>`;
          })
          .join("\n\n"),

      pageTitle: () =>
        this.page.model.kindOf(ReflectionKind.Module)
          ? this.page.model.getFullName("/")
          : pageTitle(),
    };
  }
}

function isComponent(reflection: Reflection) {
  return getComments(reflection).some((comment) =>
    comment.hasModifier(COMPONENT_TAG),
  );
}

function getComments(reflection: Reflection) {
  if (!(reflection instanceof DeclarationReflection)) return [];
  return [
    reflection.comment,
    ...(reflection.signatures ?? []).map((signature) => signature.comment),
  ].filter((comment) => comment !== undefined);
}
