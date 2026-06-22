import useBaseUrl from "@docusaurus/useBaseUrl";

interface ImgWithCaptionProps {
  source: string;
  caption?: string;
  width?: number;
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
  const isFramed = props.framed === true;
  const frameBg = props.frameBackgroundColor ?? "#F5C842";
  const frameBorderColor = props.frameBorderColor ?? "#111";
  const frameBorderWidth = props.frameBorderWidth ?? 3;
  const framePadding = props.framePadding ?? 14;

  const img = (
    <img
      style={{
        width: props.width,
        display: isFramed ? "block" : undefined,
        border: isFramed
          ? `${frameBorderWidth}px solid ${frameBorderColor}`
          : undefined,
        boxSizing: isFramed ? "border-box" : undefined,
      }}
      alt={props.alt}
      src={resolvedSrc}
    />
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
        justifyContent: props.justifyContent || "center",
      }}
    >
      <figure style={{ margin: props.margin }}>
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
