import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import BlogSidebar from '@theme/BlogSidebar';

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
export default function BlogLayout(props) {
  const {sidebar, toc, children, ...layoutProps} = props;

  // NOTE(matija): if false, then it is an individual blog post.
  //
  // NOTE(matija): I don't really like this way of determining if we're dealing with a blog post
  // or a list, since we depend on Docusaurus' internal structure, but I haven't found another way so far.
  const isListOfBlogPosts = props.children[0].props.items?.length > 0

  return (
    <Layout {...layoutProps}>
      <div
        className={clsx({
          'container margin-vert--lg': !isListOfBlogPosts
        })}
      >
        <div className="row">
          {!isListOfBlogPosts && <BlogSidebar sidebar={sidebar} />}
          <main
            className={clsx('col', {
              'col--7': !isListOfBlogPosts,
            })}
            itemScope
            itemType="http://schema.org/Blog">
            {children}
          </main>
          {toc && <div className="col col--2">{toc}</div>}
        </div>
      </div>
    </Layout>
  );
}
