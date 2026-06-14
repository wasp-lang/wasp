import Link from "@docusaurus/Link";
import "./styles.css";

const KIND_LABELS = {
  guide: "Guide",
  api: "API reference",
  docs: "Documentation",
  external: "External",
} as const;

export function CardLink({
  to,
  kind,
  title,
  description,
}: {
  to: string;
  kind?: keyof typeof KIND_LABELS;
  title: string;
  description: string;
}) {
  return (
    <Link to={to} className="card-link">
      {kind && (
        <div>
          <span className="subtitle">{KIND_LABELS[kind]}</span>
        </div>
      )}
      <h3>{title} »</h3>
      <p className="description">{description}</p>
    </Link>
  );
}
