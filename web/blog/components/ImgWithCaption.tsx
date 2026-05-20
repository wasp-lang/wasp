import useBaseUrl from "@docusaurus/useBaseUrl";

interface ImgWithCaptionProps {
  source: string;
  caption: string;
  width?: number;
  alt: string;
  justifyContent?: "center" | "flex-start" | "flex-end";
  margin?: string;
}

export function ImgWithCaption(props: ImgWithCaptionProps) {
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
        <img
          style={{ width: props.width }}
          alt={props.alt}
          src={useBaseUrl(props.source)}
        />
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
