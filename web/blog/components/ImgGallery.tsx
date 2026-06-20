import { useState, useRef, useEffect } from "react";
import useBaseUrl from "@docusaurus/useBaseUrl";

interface GalleryImage {
  source: string;
  alt: string;
  caption?: string;
}

interface ImgGalleryProps {
  images: GalleryImage[];
  caption?: string;
}

export function ImgGallery({ images, caption }: ImgGalleryProps) {
  const trackRef = useRef<HTMLDivElement>(null);
  const [activeIndex, setActiveIndex] = useState(0);

  useEffect(() => {
    const track = trackRef.current;
    if (!track) return;
    const onScroll = () => {
      const width = track.clientWidth;
      if (width === 0) return;
      const idx = Math.round(track.scrollLeft / width);
      setActiveIndex(idx);
    };
    track.addEventListener("scroll", onScroll, { passive: true });
    return () => track.removeEventListener("scroll", onScroll);
  }, []);

  const scrollTo = (idx: number) => {
    const track = trackRef.current;
    if (!track) return;
    const target = Math.max(0, Math.min(idx, images.length - 1));
    track.scrollTo({ left: target * track.clientWidth, behavior: "smooth" });
  };

  return (
    <div className="figure-container" style={{ display: "flex", flexDirection: "column", alignItems: "stretch" }}>
      <figure style={{ margin: 0, position: "relative" }}>
        <div
          ref={trackRef}
          style={{
            display: "flex",
            overflowX: "auto",
            scrollSnapType: "x mandatory",
            scrollBehavior: "smooth",
            gap: 0,
            // hide native scrollbar but keep scrolling
            scrollbarWidth: "none",
          }}
        >
          {images.map((img, i) => (
            <Slide key={i} img={img} />
          ))}
        </div>

        {/* prev / next buttons */}
        {images.length > 1 && (
          <>
            <NavButton side="left" onClick={() => scrollTo(activeIndex - 1)} disabled={activeIndex === 0} />
            <NavButton side="right" onClick={() => scrollTo(activeIndex + 1)} disabled={activeIndex === images.length - 1} />
          </>
        )}

        {/* dot indicators */}
        {images.length > 1 && (
          <div style={{ display: "flex", justifyContent: "center", gap: 8, marginTop: 12 }}>
            {images.map((_, i) => (
              <button
                key={i}
                onClick={() => scrollTo(i)}
                aria-label={`Go to slide ${i + 1}`}
                style={{
                  width: 8,
                  height: 8,
                  padding: 0,
                  border: "1.5px solid var(--ifm-color-emphasis-1000)",
                  background:
                    i === activeIndex ? "var(--ifm-color-emphasis-1000)" : "transparent",
                  cursor: "pointer",
                }}
              />
            ))}
          </div>
        )}

        {(caption || images[activeIndex]?.caption) && (
          <figcaption
            className="image-caption"
            style={{
              fontStyle: "italic",
              opacity: 0.8,
              fontSize: "1.1rem",
              marginTop: "0.4em",
              textAlign: "center",
            }}
          >
            {images[activeIndex]?.caption || caption}
          </figcaption>
        )}
      </figure>
    </div>
  );
}

function Slide({ img }: { img: GalleryImage }) {
  const src = useBaseUrl(img.source);
  return (
    <div
      style={{
        flex: "0 0 100%",
        scrollSnapAlign: "center",
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
      }}
    >
      <img src={src} alt={img.alt} style={{ maxWidth: "100%", display: "block" }} />
    </div>
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
        width: 36,
        height: 36,
        border: "1.5px solid #111",
        background: "#F5C842", // Wasp Yellow — same in both themes, brand-consistent
        color: "#111",
        cursor: disabled ? "not-allowed" : "pointer",
        opacity: disabled ? 0.3 : 1,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        fontFamily: "monospace",
        fontWeight: 700,
        fontSize: 18,
        padding: 0,
      }}
    >
      {side === "left" ? "‹" : "›"}
    </button>
  );
}
