import useBaseUrl from "@docusaurus/useBaseUrl";

interface VideoWithCaptionProps {
  source: string;
  caption?: string;
  alt?: string;
}

export function VideoWithCaption(props: VideoWithCaptionProps) {
  return (
    <div
      style={{ display: "flex", justifyContent: "center", margin: "1rem 0" }}
    >
      <figure style={{ width: "100%", margin: 0 }}>
        <video width="100%" controls aria-label={props.alt}>
          <source src={useBaseUrl(props.source)} type="video/mp4" />
        </video>
        {props.caption && (
          <figcaption
            className="image-caption"
            style={{ fontStyle: "italic", opacity: 0.6, fontSize: "0.9rem" }}
          >
            {props.caption}
          </figcaption>
        )}
      </figure>
    </div>
  );
}
