import Head from "@docusaurus/Head";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";

import ExampleWaspApps from "../components/ExampleWaspApps";
import Faq from "../components/Faq.jsx";
import Features from "../components/Features";
import Footer from "../components/Footer";
import Hero from "../components/Hero";
import HowItWorks from "../components/HowItWorks";
import Nav from "../components/Nav/index";
import Newsletter from "../components/Newsletter";
import Roadmap from "../components/Roadmap";
import ShowcaseGallery from "../components/ShowcaseGallery";
import Testimonials from "../components/Testimonials";

import waspCoverPhoto from "../../static/img/wasp_twitter_cover.png";
import "./index.css";
import "./preflight.css";

import "../prism/prismCustomization";

const Index = () => {
  const { siteConfig } = useDocusaurusContext();
  const coverPhotoAbsoluteUrl = `${siteConfig.url}${waspCoverPhoto}`;
  return (
    <div className="twLandingPage">
      <Head>
        {/* opengraph / facebook */}
        <meta property="og:type" content="website" />
        <meta property="og:url" content="https://wasp.sh/" />
        <meta
          property="og:description"
          content="Develop full-stack web apps without boilerplate."
        />
        <meta property="og:image" content={coverPhotoAbsoluteUrl} />
        {/* twitter */}
        <meta property="twitter:card" content="summary_large_image" />
        <meta property="twitter:url" content="https://wasp.sh/" />
        <meta
          property="twitter:title"
          content="Develop full-stack web apps without boilerplate."
        />
        <meta property="twitter:image" content={coverPhotoAbsoluteUrl} />
      </Head>
      <Nav />
      <div className="min-h-screen">
        <main>
          <div>
            {/* container */}
            <Hero />
            <Features />
            <HowItWorks />
            <ExampleWaspApps />
            <Testimonials />
            <ShowcaseGallery />
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
