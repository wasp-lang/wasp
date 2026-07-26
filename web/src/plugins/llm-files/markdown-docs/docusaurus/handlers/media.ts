import type * as hast from "hast";
import type * as mdast from "mdast";

/**
 * Converts an embedded iframe (e.g. a YouTube video) to a markdown link,
 * since markdown has no embeds. Iframes without a `src` are dropped.
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
 * Converts an image to a markdown image.
 *
 * Images inlined as base64 `data:` URIs are replaced with a short
 * placeholder carrying the image title. The bundler inlines small
 * images that way, and the resulting wall of base64 would only clog
 * the context of LLMs reading the markdown.
 *
 * @example
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
