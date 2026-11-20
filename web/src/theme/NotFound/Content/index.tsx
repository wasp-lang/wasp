import { useLocation } from "@docusaurus/router";
import type { WrapperProps } from "@docusaurus/types";
import { track } from "@site/src/lib/analytics";
import Content from "@theme-original/NotFound/Content";
import type ContentType from "@theme/NotFound/Content";
import { useEffect, type ReactNode } from "react";

type Props = WrapperProps<typeof ContentType>;

// This wraps `NotFound/Content`, not `NotFound`, because Docusaurus shows
// `NotFound/Content` directly for a missing docs page.
export default function ContentWrapper(props: Props): ReactNode {
  const { pathname } = useLocation();

  useEffect(() => {
    // The 404 page has no placement, so the event gets `body`.
    track(document.body, "404", { path: pathname });
  }, [pathname]);

  return <Content {...props} />;
}
