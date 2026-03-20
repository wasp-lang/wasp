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
    <div style={{ display: "flex", justifyContent: props.justifyContent || "center" }}>
      <figure style={props.margin ? { margin: props.margin } : undefined}>
        <img
          style={{ width: props.width }}
          alt={props.alt}
          src={useBaseUrl(props.source)}
        />
        <figcaption
          className="image-caption"
          style={{ fontStyle: "italic", opacity: 0.6, fontSize: "0.9rem" }}
        >
          {props.caption}
        </figcaption>
      </figure>
    </div>
  );
}
