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
    <div
      className="figure-container img-with-caption"
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
