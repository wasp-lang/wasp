import type { Props as BlogLayoutProps } from "@theme/BlogLayout";
import BlogSidebar from "@theme/BlogSidebar";
import Layout from "@theme/Layout";

// NOTE(matija): this component is used both when listing all the posts (on /blog) and when rendering a
// specific blog post.
export default function BlogLayout(props: BlogLayoutProps) {
  const { sidebar, toc, children, ...layoutProps } = props;

  // NOTE(matija): if false, then it is an individual blog post.
  //
  // FIXME(carlos): a bit hacky, but this is the only way to check if
  // the blog post is a list of blog posts.
  const isListOfBlogPosts = !toc;

  return (
    <Layout {...layoutProps}>
      {isListOfBlogPosts ? (
        <BlogPostsLayout>{children}</BlogPostsLayout>
      ) : (
        <SingleBlogPostLayout sidebar={sidebar} toc={toc}>
          {children}
        </SingleBlogPostLayout>
      )}
    </Layout>
  );
}

function BlogPostsLayout({ children }: BlogLayoutProps) {
  return <MainContent>{children}</MainContent>;
}

function SingleBlogPostLayout({ sidebar, toc, children }: BlogLayoutProps) {
  return (
    <div className="margin-vert--lg container">
      <div className="row">
        <BlogSidebar sidebar={sidebar} />
        <MainContent className="col col--7">{children}</MainContent>
        <div className="col col--2">{toc}</div>
      </div>
    </div>
  );
}

function MainContent({
  children,
  className,
}: React.PropsWithChildren<{ className?: string }>) {
  return (
    <main className={className} itemScope itemType="http://schema.org/Blog">
      {children}
    </main>
  );
}
