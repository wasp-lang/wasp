import Link from "@docusaurus/Link";

export function InBlogCta() {
  return (
    <p className="in-blog-cta-link-container">
      <Link
        className="in-blog-cta--link"
        to="https://e44cy1h4s0q.typeform.com/to/ycUzQa5A"
      >
        We are in Beta (try it out)!
      </Link>
      <CtaDivider />
      <Link className="in-blog-cta--link" to="https://discord.gg/rzdnErX">
        Join our community
      </Link>
      <CtaDivider />
      <Link
        className="in-blog-cta--link"
        to="https://wasp-lang.notion.site/Founding-Engineer-at-Wasp-402274568afa4d7eb7f428f8fa2c0816"
      >
        Work with us
      </Link>
    </p>
  );
}

function CtaDivider() {
  return <span className="in-blog-cta--divider"> → </span>;
}
