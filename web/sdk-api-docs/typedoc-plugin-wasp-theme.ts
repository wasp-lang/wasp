/**
 * TypeDoc plugin with the Wasp docs' Markdown theme, which:
 *
 * - Titles module pages with the module's full import path, e.g.
 *   `wasp/auth/providers` instead of `auth/providers`.
 * - Titles React components as `<Name>`, instead of `Name()`, and puts them in
 *   the "Components" group unless they set a `@group`. Mark a component with
 *   the `@component` modifier tag in its doc comment.
 * - Gives every heading a page-unique ID (see `addUniqueHeadingIds`).
 */

import GithubSlugger from "github-slugger";
import {
  CommentTag,
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
const DEFAULT_GROUP_TAG = "@group";

export function load(app: Application) {
  // Runs before TypeDoc groups the reflections (at priority -100).
  app.converter.on(Converter.EVENT_RESOLVE_END, (context) => {
    for (const reflection of Object.values(context.project.reflections)) {
      const comments = getComments(reflection);
      const componentComment = comments.find((comment) =>
        comment.hasModifier(COMPONENT_TAG),
      );
      if (
        componentComment &&
        !comments.some((comment) => comment.getTag(DEFAULT_GROUP_TAG))
      ) {
        componentComment.blockTags.push(
          new CommentTag(DEFAULT_GROUP_TAG, [
            { kind: "text", text: "Components" },
          ]),
        );
      }
    }
  });

  app.renderer.defineTheme("wasp-markdown", WaspMarkdownTheme);

  app.renderer.on(MarkdownPageEvent.END, (page) => {
    if (page.contents) {
      page.contents = addUniqueHeadingIds(page.contents);
    }
  });
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

      // `typedoc-plugin-markdown` shows all modifier tags, but `@component` is
      // only for this plugin.
      comment: (model, options) => {
        const modelWithoutTag = model.clone();
        modelWithoutTag.removeModifier(COMPONENT_TAG);
        return comment(modelWithoutTag, options);
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
