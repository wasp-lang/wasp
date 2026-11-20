import Head from "@docusaurus/Head";
import type { WrapperProps } from "@docusaurus/types";
import SearchPage from "@theme-original/SearchPage";
import type SearchPageType from "@theme/SearchPage";
import { type ReactNode } from "react";

type Props = WrapperProps<typeof SearchPageType>;

// The original search page means to keep itself out of search results, but it
// writes `<meta property="robots">`, which search engines ignore. They read
// only `name="robots"`.
export default function SearchPageWrapper(props: Props): ReactNode {
  return (
    <>
      <Head>
        <meta name="robots" content="noindex, follow" />
      </Head>
      <SearchPage {...props} />
    </>
  );
}
