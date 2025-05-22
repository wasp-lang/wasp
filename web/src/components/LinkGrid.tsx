import Link from "@docusaurus/Link";
import "./LinkGrid.css";

export interface LinkInfo {
  title: string;
  description?: string;
  linkTo: string;
}

export function LinkGrid({
  links,
  caption,
}: {
  links: LinkInfo[];
  caption?: string;
}) {
  return (
    <>
      <div className="link-grid-layout">
        {links.map((link) => (
          <LinkGridBox link={link} />
        ))}
      </div>
      {caption && (
        <p className="link-grid-info">
          <small>{caption}</small>
        </p>
      )}
    </>
  );
}

function LinkGridBox({
  link: { title, description, linkTo },
}: {
  link: LinkInfo;
}) {
  return (
    <Link to={linkTo} className="link-grid-box">
      <h3>{title} »</h3>
      {description && <p>{description}</p>}
    </Link>
  );
}
