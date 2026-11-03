/**
 * TypeDoc plugin with the Wasp docs' Markdown theme, which:
 *
 * - Titles module pages with the module's full import path, e.g.
 *   `wasp/auth/providers` instead of `auth/providers`.
 * - Titles React components as `<Name>`, instead of `Name()`, and puts them in
 *   the "Components" group unless they set a `@group`. Mark a component with
 *   the `@component` modifier tag in its doc comment.
 */

import {
  CommentTag,
  Converter,
  DeclarationReflection,
  ReflectionKind,
  type Application,
  type Reflection,
} from "typedoc";
import {
  MarkdownTheme,
  MarkdownThemeContext,
  type MarkdownPageEvent,
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
}

class WaspMarkdownTheme extends MarkdownTheme {
  override getRenderContext(page: MarkdownPageEvent<Reflection>) {
    return new WaspMarkdownThemeContext(this, page, this.application.options);
  }
}

class WaspMarkdownThemeContext extends MarkdownThemeContext {
  constructor(...args: ConstructorParameters<typeof MarkdownThemeContext>) {
    super(...args);
    const { comment, memberTitle, pageTitle } = this.partials;

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
          ? // The tag is escaped so MDX doesn't parse it as JSX. The explicit
            // ID keeps the anchor that TypeDoc links to.
            `\\<${model.name}\\> {#${this.router.getAnchor(model)}}`
          : memberTitle(model),

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
