import "./styles.css";

export function GuideLink({
  linkToGuide,
  title,
  description,
}: {
  linkToGuide: string;
  title: string;
  description: string;
}) {
  return (
    <a href={linkToGuide} className="guide-link">
      <div>
        <span className="subtitle">guide</span>
      </div>
      <h3>{title} »</h3>
      <p className="description">{description}</p>
    </a>
  );
}
