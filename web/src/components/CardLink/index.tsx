import Link from "@docusaurus/Link";
import "./styles.css";

export function CardLink({
  to,
  label,
  title,
  description,
}: {
  to: string;
  label?: string;
  title: string;
  description: string;
}) {
  return (
    <Link to={to} className="card-link">
      {label && (
        <div>
          <span className="subtitle">{label}</span>
        </div>
      )}
      <h3>{title} »</h3>
      <p className="description">{description}</p>
    </Link>
  );
}
