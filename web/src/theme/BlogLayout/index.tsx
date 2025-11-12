import type { Props } from "@theme/BlogLayout";
import BlogSidebar from "@theme/BlogSidebar";
import Layout from "@theme/Layout";

// NOTE(matija): this component is used both when listing all the posts (on /blog) and when rendering a
// specific blog post.
export default function BlogLayout(props: Props) {
  const { sidebar, toc, children, ...layoutProps } = props;

  // NOTE(matija): if false, then it is an individual blog post.
  //
  // FIXME(carlos): a bit hacky, but this is the only way to check if
  // the blog post is a list of blog posts.
  const isListOfBlogPosts = !toc;

  const seoSchemaOrgData = {
    itemScope: true,
    itemType: "http://schema.org/Blog",
  };

  return (
    <Layout {...layoutProps}>
      {isListOfBlogPosts ? (
        <main {...seoSchemaOrgData}>{children}</main>
      ) : (
        <div className="margin-vert--lg container">
          <div className="row">
            <BlogSidebar sidebar={sidebar} />
            <main className="col col--7" {...seoSchemaOrgData}>
              {children}
            </main>
            <div className="col col--2">{toc}</div>
          </div>
        </div>
      )}
    </Layout>
  );
}
