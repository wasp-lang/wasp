import { useState, useRef, useEffect } from "react";
import useBaseUrl from "@docusaurus/useBaseUrl";
import { Lightbox } from "./Lightbox";

interface GalleryImage {
  source: string;
  alt: string;
  caption?: string;
}

interface LandingPagesGridProps {
  images: GalleryImage[];
  caption?: string;
}

const DESKTOP_BREAKPOINT = 768; // px

export function LandingPagesGrid({ images, caption }: LandingPagesGridProps) {
  const [isDesktop, setIsDesktop] = useState(true);
  const [zoomed, setZoomed] = useState<{ src: string; alt: string } | null>(null);

  useEffect(() => {
    const mq = window.matchMedia(`(min-width: ${DESKTOP_BREAKPOINT}px)`);
    setIsDesktop(mq.matches);
    const onChange = (e: MediaQueryListEvent) => setIsDesktop(e.matches);
    mq.addEventListener("change", onChange);
    return () => mq.removeEventListener("change", onChange);
  }, []);

  return (
    <div className="figure-container" style={{ display: "flex", flexDirection: "column", alignItems: "stretch" }}>
      <figure style={{ margin: 0, position: "relative" }}>
        {isDesktop ? (
          <DesktopGrid images={images} onZoom={(src, alt) => setZoomed({ src, alt })} />
        ) : (
          <MobileCarousel images={images} />
        )}
        {caption && (
          <figcaption
            className="image-caption"
            style={{
              fontStyle: "italic",
              opacity: 0.8,
              fontSize: "1.1rem",
              marginTop: "0.6em",
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

function DesktopGrid({
  images,
  onZoom,
}: {
  images: GalleryImage[];
  onZoom: (src: string, alt: string) => void;
}) {
  return (
    <div style={{ display: "grid", gridTemplateColumns: "repeat(2, 1fr)", gap: 8 }}>
      {images.map((img, i) => (
        <GridImage key={i} img={img} onZoom={onZoom} />
      ))}
    </div>
  );
}

function GridImage({
  img,
  onZoom,
}: {
  img: GalleryImage;
  onZoom: (src: string, alt: string) => void;
}) {
  const src = useBaseUrl(img.source);
  return (
    <img
      src={src}
      alt={img.alt}
      className="no-default-zoom"
      onClick={() => onZoom(src, img.alt)}
      style={{
        width: "100%",
        height: "auto",
        display: "block",
        cursor: "zoom-in",
        border: "1px solid var(--wasp-g5)",
      }}
    />
  );
}

function MobileCarousel({ images }: { images: GalleryImage[] }) {
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
    <div style={{ position: "relative" }}>
      <div
        ref={trackRef}
        style={{
          display: "flex",
          overflowX: "auto",
          scrollSnapType: "x mandatory",
          scrollBehavior: "smooth",
          gap: 0,
          scrollbarWidth: "none",
        }}
      >
        {images.map((img, i) => (
          <MobileSlide key={i} img={img} />
        ))}
      </div>
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
    </div>
  );
}

function MobileSlide({ img }: { img: GalleryImage }) {
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
      <img src={src} alt={img.alt} className="no-default-zoom" style={{ maxWidth: "100%", display: "block" }} />
    </div>
  );
}

