import Link from "@docusaurus/Link";
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
    <Link to={linkToGuide} className="guide-link">
      <div>
        <span className="subtitle">guide</span>
      </div>
      <h3>{title} »</h3>
      <p className="description">{description}</p>
    </Link>
  );
}
