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

// TypeDoc lists the members of a group with this name without a heading.
const NO_GROUP = "none";

/**
 * TypeDoc plugin that lists each module's exports without TypeDoc's default
 * groups (e.g. "Functions" or "Type Aliases"), except for the types documented
 * by `typedoc-plugin-missing-exports`, which go into the
 * {@link API_DOCS_REFERENCED_TYPES_GROUP} group.
 */
export function load(app: Application): void {
  let isDocumentingMissingExports = false;
  const referencedTypes = new Set<DeclarationReflection>();

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
      if (isDocumentingMissingExports && isModuleExport(reflection)) {
        referencedTypes.add(reflection);
      }
    },
  );

  app.converter.on(
    Converter.EVENT_RESOLVE_BEGIN,
    (context) => {
      isDocumentingMissingExports = false;

      for (const reflection of Object.values(context.project.reflections)) {
        if (
          !reflection.isDeclaration() ||
          !isModuleExport(reflection) ||
          reflection.kindOf(ReflectionKind.Module) ||
          hasGroupTag(reflection)
        ) {
          continue;
        }

        if (referencedTypes.has(reflection)) {
          // Only categorize them in modules that use categories, otherwise
          // TypeDoc would list the module's exports by category instead.
          addTag(reflection, "@group", API_DOCS_REFERENCED_TYPES_GROUP);
          if (usesCategories(reflection.parent)) {
            addTag(reflection, "@category", API_DOCS_REFERENCED_TYPES_GROUP);
          }
        } else {
          addTag(reflection, "@group", NO_GROUP);
        }
      }

      referencedTypes.clear();
    },
    MISSING_EXPORTS_PLUGIN_PRIORITY - 1,
  );
}

function isModuleExport(reflection: Reflection): boolean {
  return (
    reflection.parent?.kindOf(ReflectionKind.Project | ReflectionKind.Module) ??
    false
  );
}

function addTag(
  reflection: DeclarationReflection,
  tag: `@${string}`,
  text: string,
): void {
  reflection.comment ??= new Comment();
  reflection.comment.blockTags.push(
    new CommentTag(tag, [{ kind: "text", text }]),
  );
}

function hasGroupTag(reflection: DeclarationReflection): boolean {
  return getComments(reflection).some((comment) => comment.getTag("@group"));
}

function usesCategories(module: Reflection | undefined): boolean {
  if (!module?.isProject() && !module?.isDeclaration()) {
    return false;
  }

  return (module.children ?? []).some((child) =>
    getComments(child).some((comment) => comment.getTag("@category")),
  );
}

function getComments(reflection: DeclarationReflection): Comment[] {
  return [
    reflection.comment,
    ...(reflection.signatures ?? []).map((signature) => signature.comment),
  ].filter((comment) => comment !== undefined);
}
