import Head from "@docusaurus/Head";

import AI from "../components/AI";
import ExampleApps from "../components/ExampleApps";
import Faq from "../components/Faq";
import FeaturesExplorer from "../components/FeaturesExplorer";
import Footer from "../components/Footer";
import Hero from "../components/Hero";
import HowItWorks from "../components/HowItWorks";
import Nav from "../components/Nav/index";
import Newsletter from "../components/Newsletter";
import NoLockIn from "../components/NoLockIn";
import OpenSaas from "../components/OpenSaas";
import Philosophy from "../components/Philosophy";
import Properties from "../components/Properties";
import Roadmap from "../components/Roadmap";
import ShippedWithWasp from "../components/ShippedWithWasp";
import Testimonials from "../components/Testimonials";
import Waspeteers from "../components/Waspeteers";

import "./index.css";
import "./preflight.css";

import "../prism/prismCustomization";

const homepageTitle =
  "Wasp: full-stack TypeScript framework for React and Node.js";
const homepageDescription =
  "Wasp is a full-stack TypeScript framework wiring React, Node.js and Prisma together, with auth, jobs and deployment built in, like Rails or Laravel.";

// The site name and the organization behind it. Keep `name`, `url` and `logo`
// the same as the publisher in `theme/BlogPostPage/StructuredData`.
const homepageStructuredData = {
  "@context": "https://schema.org",
  "@graph": [
    {
      "@type": "WebSite",
      "@id": "https://wasp.sh/#website",
      name: "Wasp",
      alternateName: ["wasp.sh", "Wasp framework"],
      url: "https://wasp.sh/",
      publisher: { "@id": "https://wasp.sh/#organization" },
    },
    {
      "@type": "Organization",
      "@id": "https://wasp.sh/#organization",
      name: "Wasp",
      url: "https://wasp.sh",
      logo: {
        "@type": "ImageObject",
        url: "https://wasp.sh/img/wasp-logo.png",
      },
      sameAs: [
        "https://github.com/wasp-lang",
        "https://x.com/WaspLang",
        "https://bsky.app/profile/wasp.sh",
      ],
    },
  ],
};

const Index = () => {
  return (
    <div className="twLandingPage">
      <Head>
        <title>{homepageTitle}</title>
        <meta name="description" content={homepageDescription} />
        {/* opengraph / facebook */}
        <meta property="og:type" content="website" />
        <meta property="og:url" content="https://wasp.sh/" />
        <meta property="og:title" content={homepageTitle} />
        <meta property="og:description" content={homepageDescription} />
        {/* twitter */}
        <meta property="twitter:url" content="https://wasp.sh/" />
        <meta property="twitter:title" content={homepageTitle} />
        <meta property="twitter:description" content={homepageDescription} />
        <script type="application/ld+json">
          {JSON.stringify(homepageStructuredData)}
        </script>
      </Head>
      <Nav />
      <div className="min-h-screen">
        <main>
          <div>
            {/* container */}
            <Hero />
            <Properties />
            <FeaturesExplorer />
            <AI />
            <HowItWorks />
            <NoLockIn />
            <ExampleApps />
            <ShippedWithWasp />
            <Testimonials />
            <Waspeteers />
            <Philosophy />
            <OpenSaas />
            <Newsletter />
            <Roadmap />
            <Faq />
          </div>
          {/* eof container */}
        </main>
      </div>
      <Footer />
    </div>
  );
};

export default Index;
