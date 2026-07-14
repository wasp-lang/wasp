import { useEffect } from "react";

interface LightboxProps {
  src: string;
  alt: string;
  onClose: () => void;
}

/**
 * Full-screen image overlay used by both the Carousel and the ImgGallery
 * in this post. Owns the two concerns every lightbox needs:
 *   - Esc key closes.
 *   - Body scroll is locked while open.
 * The `.no-default-zoom` class opts the image out of the site-wide
 * medium-zoom plugin so it doesn't fire a second overlay behind this one.
 */
export function Lightbox({ src, alt, onClose }: LightboxProps) {
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (e.key === "Escape") onClose();
    };
    window.addEventListener("keydown", onKey);
    const prev = document.body.style.overflow;
    document.body.style.overflow = "hidden";
    return () => {
      window.removeEventListener("keydown", onKey);
      document.body.style.overflow = prev;
    };
  }, [onClose]);

  return (
    <div
      onClick={onClose}
      role="dialog"
      aria-modal="true"
      style={{
        position: "fixed",
        inset: 0,
        background: "rgba(0,0,0,0.85)",
        zIndex: 1000,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        cursor: "zoom-out",
        padding: 24,
      }}
    >
      <img
        src={src}
        alt={alt}
        className="no-default-zoom"
        onClick={(e) => e.stopPropagation()}
        style={{
          maxWidth: "95vw",
          maxHeight: "95vh",
          display: "block",
          cursor: "default",
        }}
      />
    </div>
  );
}
