// JSON-LD structured data for SEO. Helps search engines and LLMs understand
// your app so it can appear in rich results and AI answers. Customize the
// placeholders below to match your product. See https://schema.org for types.
const schema = {
  "@context": "https://schema.org",
  "@graph": [
    {
      "@type": "SoftwareApplication",
      "@id": "https://your-saas-app.com/#software",
      name: "Your Open SaaS App",
      description: "Your apps main description and features.",
      url: "https://your-saas-app.com",
      applicationCategory: "BusinessApplication",
      operatingSystem: "Cross-platform",
      image: "https://your-saas-app.com/public-banner.webp",
      offers: {
        "@type": "Offer",
        price: "0",
        priceCurrency: "USD",
      },
    },
    {
      "@type": "WebSite",
      "@id": "https://your-saas-app.com/#website",
      url: "https://your-saas-app.com",
      name: "Your Open SaaS App",
      description: "Your apps main description and features.",
    },
  ],
};

export function SchemaMarkup() {
  return <script type="application/ld+json">{JSON.stringify(schema)}</script>;
}
