import Head from "@docusaurus/Head";
import type { WrapperProps } from "@docusaurus/types";
import BlogTagsPostsPage from "@theme-original/BlogTagsPostsPage";
import type BlogTagsPostsPageType from "@theme/BlogTagsPostsPage";
import { type ReactNode } from "react";

type Props = WrapperProps<typeof BlogTagsPostsPageType>;

// A tag page (e.g. /blog/tags/react) only lists posts that have their own
// pages, so we keep it out of search results. "follow" lets crawlers still
// reach the posts through its links.
export default function BlogTagsPostsPageWrapper(props: Props): ReactNode {
  return (
    <>
      <Head>
        <meta name="robots" content="noindex, follow" />
      </Head>
      <BlogTagsPostsPage {...props} />
    </>
  );
}
