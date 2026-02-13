import Head from "@docusaurus/Head";
import { useBlogPostStructuredData } from "@docusaurus/plugin-content-blog/client";
import { type ReactNode } from "react";

export default function BlogPostStructuredData(): ReactNode {
  const structuredData = useBlogPostStructuredData();

  const structuredDataWithPublisher = {
    ...structuredData,
    mainEntityOfPage: {
      "@type": "WebPage",
      "@id": structuredData.mainEntityOfPage,
    },
    publisher: {
      "@type": "Organization",
      name: "Wasp",
      url: "https://wasp.sh",
      logo: {
        "@type": "ImageObject",
        url: "https://wasp.sh/img/wasp-logo.png",
      },
    },
  };

  return (
    <Head>
      <script type="application/ld+json">
        {JSON.stringify(structuredDataWithPublisher)}
      </script>
    </Head>
  );
}
