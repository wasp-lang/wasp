import Head from "@docusaurus/Head";
import useBrokenLinks from "@docusaurus/useBrokenLinks";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import classNames from "classnames";

import Footer from "../components/Footer";
import Nav from "../components/Nav/index";
import VCBenchmark from "../components/vibeCoding/VCBenchmark";
import VCFAQ from "../components/vibeCoding/VCFAQ";
import VCFeatures from "../components/vibeCoding/VCFeatures";
import VCFinalCTA from "../components/vibeCoding/VCFinalCTA";
import VCHero from "../components/vibeCoding/VCHero";
import VCTestimonials from "../components/vibeCoding/VCTestimonials";
import VCExamples from "../components/vibeCoding/VCExamples";
import VCWhyWasp from "../components/vibeCoding/VCWhyWasp";
import VCWorkflow from "../components/vibeCoding/VCWorkflow";

import waspCoverPhoto from "../../static/img/wasp_twitter_cover.png";
import "./index.css";
import "./preflight.css";
import styles from "./styles.module.css";
import VCCodeComparison from "../components/vibeCoding/VCCodeComparison";

const Background = () => {
  return (
    <div className="pointer-events-none absolute left-0 top-0 h-full w-full overflow-hidden">
      <span className={classNames(styles.leftLights, "opacity-100")} />
    </div>
  );
};

const LightsTwo = () => (
  <div className="pointer-events-none absolute left-0 top-[1800px] h-full w-full overflow-hidden lg:top-[1000px]">
    <span className={classNames(styles.lightsTwo, "opacity-100")} />
  </div>
);

const VibeCoding = () => {
  const { siteConfig } = useDocusaurusContext();
  const coverPhotoAbsoluteUrl = `${siteConfig.url}${waspCoverPhoto}`;

  // Register anchors referenced by the shared Nav component
  useBrokenLinks().collectAnchor("faq");
  useBrokenLinks().collectAnchor("signup");

  return (
    <div className="twLandingPage">
      <Head>
        <title>Wasp: The Framework Built for Vibe Coding</title>
        {/* opengraph / facebook */}
        <meta property="og:type" content="website" />
        <meta property="og:url" content="https://wasp.sh/vibe-coding" />
        <meta
          property="og:title"
          content="Wasp: The Framework Built for Vibe Coding"
        />
        <meta
          property="og:description"
          content="The full-stack framework where AI writes business logic, not boilerplate."
        />
        <meta property="og:image" content={coverPhotoAbsoluteUrl} />
        {/* twitter */}
        <meta property="twitter:card" content="summary_large_image" />
        <meta property="twitter:url" content="https://wasp.sh/vibe-coding" />
        <meta
          property="twitter:title"
          content="Wasp: The Framework Built for Vibe Coding"
        />
        <meta
          property="twitter:description"
          content="The full-stack framework where AI writes business logic, not boilerplate."
        />
        <meta property="twitter:image" content={coverPhotoAbsoluteUrl} />
      </Head>
      <Nav />
      <div className="min-h-screen">
        <main>
          {/* <Background /> */}
          <div className="relative">
            <LightsTwo />
            <div className="relative z-10">
              <VCHero />
              <VCCodeComparison />
              <VCBenchmark />
              <VCFeatures />
              <VCWhyWasp />
              <VCWorkflow />
              <VCExamples />
              <VCTestimonials />
              <VCFAQ />
              <VCFinalCTA />
            </div>
          </div>
        </main>
      </div>
      <Footer />
    </div>
  );
};

export default VibeCoding;
