import type * as hast from "hast";
import type * as mdast from "mdast";

/**
 * Converts an `<iframe>` element (e.g. a YouTube video) to a markdown link.
 * (markdown has no embeds). Iframes without a `src` are dropped.
 *
 * @example
 * Source HTML:
 * ```html
 * <iframe src="https://www.youtube.com/embed/Qiro77q-ulI" title="Wasp auth tour" />
 * ```
 * Generated markdown:
 * ```md
 * [Wasp auth tour](https://www.youtube.com/embed/Qiro77q-ulI)
 * ```
 */
export function iframeToMdast(iframe: hast.Element): mdast.Link | undefined {
  const src = iframe.properties?.src;
  if (typeof src !== "string" || !src) {
    return undefined;
  }

  const title = iframe.properties?.title;
  const label = typeof title === "string" && title ? title : "Embedded content";

  return {
    type: "link",
    url: src,
    children: [{ type: "text", value: label }],
  };
}

/**
 * Converts an `<img>` element to a markdown image.
 *
 * Bundler inlines small images as base64 `data:` which results
 * in a lot of "garbage" data. To avoid clogging up the LLM context,
 * we replace them with small placeholders.

 * @example
 * Inlined image placeholder:
 * ```md
 * *(Inlined image: Forgot password form)*
 * ```
 */
export function imageToMdast(
  image: hast.Element,
): mdast.Image | mdast.Emphasis | undefined {
  const src = image.properties?.src;
  if (typeof src !== "string" || !src) {
    return undefined;
  }

  const alt = image.properties?.alt;
  const title = image.properties?.title;
  const imageTitle =
    (typeof alt === "string" && alt) ||
    (typeof title === "string" && title) ||
    undefined;

  if (src.startsWith("data:")) {
    return inlinedImagePlaceholder(imageTitle);
  }

  return {
    type: "image",
    url: src,
    alt: typeof alt === "string" ? alt : undefined,
    title: typeof title === "string" ? title : undefined,
  };
}

function inlinedImagePlaceholder(
  imageTitle: string | undefined,
): mdast.Emphasis {
  const label = imageTitle
    ? `(Inlined image: ${imageTitle})`
    : "(Inlined image)";
  return {
    type: "emphasis",
    children: [{ type: "text", value: label }],
  };
}
