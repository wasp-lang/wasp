import Head from "@docusaurus/Head";
import type { WrapperProps } from "@docusaurus/types";
import BlogTagsListPage from "@theme-original/BlogTagsListPage";
import type BlogTagsListPageType from "@theme/BlogTagsListPage";
import { type ReactNode } from "react";

type Props = WrapperProps<typeof BlogTagsListPageType>;

// The list of all tags (e.g. /blog/tags) has no content of its own, so we keep
// it out of search results, like the tag pages in ../BlogTagsPostsPage.
export default function BlogTagsListPageWrapper(props: Props): ReactNode {
  return (
    <>
      <Head>
        <meta name="robots" content="noindex, follow" />
      </Head>
      <BlogTagsListPage {...props} />
    </>
  );
}
