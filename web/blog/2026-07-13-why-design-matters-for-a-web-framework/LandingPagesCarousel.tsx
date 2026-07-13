import { useState, useEffect, useRef } from "react";
import useBaseUrl from "@docusaurus/useBaseUrl";
import { Lightbox } from "./Lightbox";

interface CarouselImage {
  source: string;
  alt: string;
  label?: string;
  framed?: boolean;
}

interface CarouselProps {
  images: CarouselImage[];
  caption?: string;
}

export function LandingPagesCarousel({ images, caption }: CarouselProps) {
  const [index, setIndex] = useState(0);
  const [zoomed, setZoomed] = useState<{ src: string; alt: string } | null>(null);
  const rootRef = useRef<HTMLDivElement>(null);

  const clamp = (i: number) => Math.max(0, Math.min(images.length - 1, i));
  const go = (delta: number) => setIndex((i) => clamp(i + delta));
  const jump = (i: number) => setIndex(clamp(i));

  // Arrow keys navigate when the carousel is focused. Lightbox handles its own Esc.
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (zoomed) return; // let Lightbox handle its own keys
      if (!rootRef.current?.contains(document.activeElement)) return;
      if (e.key === "ArrowLeft") { e.preventDefault(); go(-1); }
      if (e.key === "ArrowRight") { e.preventDefault(); go(1); }
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [zoomed, images.length]);

  const current = images[index];

  return (
    <div
      className="figure-container"
      ref={rootRef}
      tabIndex={0}
      style={{ display: "flex", flexDirection: "column", alignItems: "stretch", outline: "none" }}
    >
      <figure style={{ margin: 0, position: "relative" }}>
        {/* Label + counter above the current slide */}
        {(current.label || images.length > 1) && (
          <div style={{ display: "flex", justifyContent: "space-between", alignItems: "baseline", marginBottom: 8 }}>
            <div style={{ fontFamily: "IBM Plex Mono, monospace", fontWeight: 600, fontSize: 15 }}>
              {current.label}
            </div>
            {images.length > 1 && (
              <div style={{
                fontFamily: "IBM Plex Mono, monospace",
                fontSize: 12,
                letterSpacing: "0.08em",
                opacity: 0.6,
              }}>
                {String(index + 1).padStart(2, "0")} / {String(images.length).padStart(2, "0")}
              </div>
            )}
          </div>
        )}

        {/* Track */}
        <div style={{ position: "relative", overflow: "hidden" }}>
          <div
            style={{
              display: "flex",
              transform: `translateX(-${index * 100}%)`,
              transition: "transform 0.35s ease",
            }}
          >
            {images.map((img, i) => (
              <Slide key={i} img={img} onZoom={(src, alt) => setZoomed({ src, alt })} />
            ))}
          </div>

          {images.length > 1 && (
            <>
              <NavButton side="left" onClick={() => go(-1)} disabled={index === 0} />
              <NavButton side="right" onClick={() => go(1)} disabled={index === images.length - 1} />
            </>
          )}
        </div>

        {/* Thumbnail strip */}
        {images.length > 1 && (
          <div style={{ display: "flex", justifyContent: "center", flexWrap: "wrap", gap: 8, marginTop: 16 }}>
            {images.map((img, i) => (
              <Thumbnail
                key={i}
                img={img}
                active={i === index}
                onClick={() => jump(i)}
                label={`Go to slide ${i + 1}${img.label ? `: ${img.label}` : ""}`}
              />
            ))}
          </div>
        )}

        {caption && (
          <figcaption
            className="image-caption"
            style={{
              fontStyle: "italic",
              opacity: 0.8,
              fontSize: "1.1rem",
              marginTop: "0.8em",
              textAlign: "center",
            }}
          >
            {caption}
          </figcaption>
        )}
      </figure>

      {zoomed && <Lightbox src={zoomed.src} alt={zoomed.alt} onClose={() => setZoomed(null)} />}
    </div>
  );
}

function Slide({
  img,
  onZoom,
}: {
  img: CarouselImage;
  onZoom: (src: string, alt: string) => void;
}) {
  const src = useBaseUrl(img.source);
  return (
    <div style={{ flex: "0 0 100%", display: "flex", justifyContent: "center", alignItems: "center" }}>
      <img
        src={src}
        alt={img.alt}
        className="no-default-zoom"
        onClick={() => onZoom(src, img.alt)}
        style={{
          maxWidth: "100%",
          display: "block",
          cursor: "zoom-in",
          background: img.framed ? "var(--wasp-y)" : undefined,
          border: img.framed ? "3px solid var(--wasp-k)" : "1px solid var(--wasp-g5)",
          padding: img.framed ? 14 : undefined,
          boxSizing: "border-box",
        }}
      />
    </div>
  );
}

function Thumbnail({
  img,
  active,
  onClick,
  label,
}: {
  img: CarouselImage;
  active: boolean;
  onClick: () => void;
  label: string;
}) {
  const src = useBaseUrl(img.source);
  return (
    <button
      type="button"
      onClick={(e) => {
        e.stopPropagation();
        e.preventDefault();
        onClick();
      }}
      aria-label={label}
      aria-current={active}
      title={img.label}
      style={{
        width: 150,
        height: 94,
        padding: 0,
        border: active ? "2.5px solid var(--wasp-y)" : "1.5px solid var(--wasp-g5)",
        outline: active ? "1px solid var(--wasp-k)" : "none",
        background: "transparent",
        cursor: "pointer",
        overflow: "hidden",
        transition: "border-color 0.15s, transform 0.15s",
        transform: active ? "scale(1.05)" : "none",
        boxSizing: "border-box",
      }}
    >
      <img
        src={src}
        alt=""
        className="no-default-zoom"
        draggable={false}
        style={{
          width: "100%",
          height: "100%",
          objectFit: "cover",
          objectPosition: "top center",
          display: "block",
          pointerEvents: "none",
        }}
      />
    </button>
  );
}

function NavButton({
  side,
  onClick,
  disabled,
}: {
  side: "left" | "right";
  onClick: () => void;
  disabled?: boolean;
}) {
  return (
    <button
      onClick={onClick}
      disabled={disabled}
      aria-label={side === "left" ? "Previous" : "Next"}
      style={{
        position: "absolute",
        top: "50%",
        [side]: 8,
        transform: "translateY(-50%)",
        width: 40,
        height: 40,
        border: "1.5px solid var(--wasp-k)",
        background: "var(--wasp-y)",
        color: "var(--wasp-k)",
        cursor: disabled ? "not-allowed" : "pointer",
        opacity: disabled ? 0.35 : 1,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        fontFamily: "monospace",
        fontWeight: 700,
        fontSize: 20,
        padding: 0,
        zIndex: 5,
      }}
    >
      {side === "left" ? "‹" : "›"}
    </button>
  );
}

