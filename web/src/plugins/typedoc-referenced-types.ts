import {
  Comment,
  CommentTag,
  Converter,
  ReflectionKind,
  type Application,
  type DeclarationReflection,
  type Reflection,
} from "typedoc";
// TypeDoc loads this plugin with Node.js directly, which needs the extension.
import { API_DOCS_REFERENCED_TYPES_GROUP } from "../lib/constants.ts";

// `typedoc-plugin-missing-exports` documents the missing exports in a
// `RESOLVE_BEGIN` listener with this priority.
const MISSING_EXPORTS_PLUGIN_PRIORITY = 1e9;

/**
 * TypeDoc plugin that puts the types documented by
 * `typedoc-plugin-missing-exports` into the
 * {@link API_DOCS_REFERENCED_TYPES_GROUP} group.
 */
export function load(app: Application): void {
  let isDocumentingMissingExports = false;
  const referencedTypes: DeclarationReflection[] = [];

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
        referencedTypes.push(reflection);
      }
    },
  );

  app.converter.on(
    Converter.EVENT_RESOLVE_BEGIN,
    () => {
      isDocumentingMissingExports = false;
      for (const reflection of referencedTypes.splice(0)) {
        // Only categorize them in modules that use categories, otherwise
        // TypeDoc would list the module's exports by category instead of by
        // kind.
        const tags = usesCategories(reflection.parent)
          ? (["@group", "@category"] as const)
          : (["@group"] as const);

        reflection.comment ??= new Comment();
        for (const tag of tags) {
          reflection.comment.blockTags.push(
            new CommentTag(tag, [
              { kind: "text", text: API_DOCS_REFERENCED_TYPES_GROUP },
            ]),
          );
        }
      }
    },
    MISSING_EXPORTS_PLUGIN_PRIORITY - 1,
  );
}

function usesCategories(module: Reflection | undefined): boolean {
  if (!module?.isProject() && !module?.isDeclaration()) {
    return false;
  }

  return (module.children ?? []).some((child) =>
    [child.comment, ...(child.signatures ?? []).map((s) => s.comment)].some(
      (comment) => comment?.getTag("@category"),
    ),
  );
}
