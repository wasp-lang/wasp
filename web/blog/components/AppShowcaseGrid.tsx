import useBaseUrl from "@docusaurus/useBaseUrl";

interface ShowcaseApp {
  src: string;
  alt: string;
  href?: string;
}

interface AppShowcaseGridProps {
  apps: ShowcaseApp[];
  backgroundColor?: string;
  borderColor?: string;
  borderWidth?: number;
  padding?: number;
  gap?: number;
}

function Tile({
  app,
  borderColor,
  borderWidth,
}: {
  app: ShowcaseApp;
  borderColor: string;
  borderWidth: number;
}) {
  const resolvedSrc = useBaseUrl(app.src);
  const img = (
    <img
      alt={app.alt}
      src={resolvedSrc}
      style={{
        display: "block",
        width: "100%",
        height: "auto",
        border: `${borderWidth}px solid ${borderColor}`,
        boxSizing: "border-box",
      }}
    />
  );
  return app.href ? (
    <a href={app.href} target="_blank" rel="noreferrer noopener" style={{ display: "block" }}>
      {img}
    </a>
  ) : (
    img
  );
}

export function AppShowcaseGrid({
  apps,
  backgroundColor = "#F5C842",
  borderColor = "#111",
  borderWidth = 3,
  padding = 14,
  gap = 12,
}: AppShowcaseGridProps) {
  return (
    <div className="figure-container">
      <div
        className="app-showcase-grid"
        style={{
          background: backgroundColor,
          padding: `${padding}px`,
          display: "grid",
          gridTemplateColumns: "1fr 1fr",
          gap: `${gap}px`,
          border: `${borderWidth}px solid ${borderColor}`,
          boxSizing: "border-box",
        }}
      >
        {apps.map((app, i) => (
          <Tile key={i} app={app} borderColor={borderColor} borderWidth={borderWidth} />
        ))}
      </div>
      <style>{`
        @media (max-width: 640px) {
          .app-showcase-grid {
            grid-template-columns: 1fr !important;
          }
        }
      `}</style>
    </div>
  );
}
