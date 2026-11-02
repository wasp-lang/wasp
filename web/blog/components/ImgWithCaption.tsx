import useBaseUrl from "@docusaurus/useBaseUrl";
import ThemedImage from "@theme/ThemedImage";
import type { ReactNode } from "react";

interface ImgWithCaptionProps {
  source: string;
  /** Optional image shown instead of `source` when the site is in dark mode. */
  sourceDark?: string;
  caption?: ReactNode;
  width?: number | string;
  height?: number | string;
  alt: string;
  justifyContent?: "center" | "flex-start" | "flex-end";
  margin?: string;
  /**
   * Optional "framed" mode — wraps the image in a colored, bordered box (e.g. the
   * Wasp yellow / black-bordered look used in showcase posts). Omit `framed` for
   * the unframed default look used by every existing post.
   */
  framed?: boolean;
  frameBackgroundColor?: string;
  frameBorderColor?: string;
  frameBorderWidth?: number;
  framePadding?: number;
}

export function ImgWithCaption(props: ImgWithCaptionProps) {
  const resolvedSrc = useBaseUrl(props.source);
  const resolvedSrcDark = useBaseUrl(props.sourceDark ?? props.source);
  const isFramed = props.framed === true;
  const frameBg = props.frameBackgroundColor ?? "#F5C842";
  const frameBorderColor = props.frameBorderColor ?? "#111";
  const frameBorderWidth = props.frameBorderWidth ?? 3;
  const framePadding = props.framePadding ?? 14;
  const justifyContent = props.justifyContent ?? "center";
  const textAlign = (
    { center: "center", "flex-start": "left", "flex-end": "right" } as const
  )[justifyContent];

  const imgStyle = {
    width: props.width,
    height: props.height,
    display: isFramed ? "block" : undefined,
    border: isFramed
      ? `${frameBorderWidth}px solid ${frameBorderColor}`
      : undefined,
    boxSizing: isFramed ? "border-box" : undefined,
  } as const;

  const img = props.sourceDark ? (
    <ThemedImage
      style={imgStyle}
      alt={props.alt}
      sources={{ light: resolvedSrc, dark: resolvedSrcDark }}
    />
  ) : (
    <img style={imgStyle} alt={props.alt} src={resolvedSrc} />
  );

  return (
    // The `figure-container` class plugs this component into the docs/blog
    // vertical rhythm system. Margins are defined in custom.css and scale
    // with the `--lh` knob (along with heading margins) — keeping figure
    // spacing consistent with the rest of the content.
    <div
      className="figure-container"
      style={{
        display: "flex",
        justifyContent,
      }}
    >
      {/* A long caption can make the figure as wide as the column, so the
          image must also be aligned inside the figure, not just the figure
          inside the container. */}
      <figure style={{ margin: props.margin, textAlign }}>
        {isFramed ? (
          <div
            style={{
              background: frameBg,
              padding: `${framePadding}px`,
              border: `${frameBorderWidth}px solid ${frameBorderColor}`,
              boxSizing: "border-box",
              display: "inline-block",
            }}
          >
            {img}
          </div>
        ) : (
          img
        )}
        <figcaption
          className="image-caption"
          style={{
            fontStyle: "italic",
            opacity: 0.8,
            fontSize: "1.1rem",
            marginTop: "0.2em",
          }}
        >
          {props.caption}
        </figcaption>
      </figure>
    </div>
  );
}
