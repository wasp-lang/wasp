import type { Props } from "@theme/BlogLayout";
import BlogSidebar from "@theme/BlogSidebar";
import Layout from "@theme/Layout";
import clsx from "clsx";

/*

            className={clsx('col', {
              'col--7': hasSidebar,
              'col--9 col--offset-1': !hasSidebar,
            })}
            className={`
              container
              grid grid-cols-1 xl:grid-cols-3 gap-3
            `}

      <div className="container margin-vert--lg">
              'col--10 col--offset-1': !hasSidebar,

*/

// NOTE(matija): this component is used both when listing all the posts (on /blog) and when rendering a
// specific blog post.
export default function BlogLayout(props: Props) {
  const { sidebar, toc, children, ...layoutProps } = props;

  // NOTE(matija): if false, then it is an individual blog post.
  //
  // FIXME(carlos): a bit hacky, but this is the only way to check if
  // the blog post is a list of blog posts.
  const isListOfBlogPosts = !toc;

  return (
    <Layout {...layoutProps}>
      <div
        className={clsx({
          "margin-vert--lg container": !isListOfBlogPosts,
        })}
      >
        <div className="row">
          {!isListOfBlogPosts && <BlogSidebar sidebar={sidebar} />}
          <main
            className={clsx("col", {
              "col--7": !isListOfBlogPosts,
            })}
            itemScope
            itemType="http://schema.org/Blog"
          >
            {children}
          </main>
          {toc && <div className="col col--2">{toc}</div>}
        </div>
      </div>
    </Layout>
  );
}
