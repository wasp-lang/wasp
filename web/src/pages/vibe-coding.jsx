import Head from "@docusaurus/Head";
import useBrokenLinks from "@docusaurus/useBrokenLinks";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import classNames from "classnames";

import Footer from "../components/Footer";
import Nav from "../components/Nav/index";
import VCBenchmark from "../components/vibeCoding/VCBenchmark";
import VCCodeComparison from "../components/vibeCoding/VCCodeComparison";
import VCExamples from "../components/vibeCoding/VCExamples";
import VCFAQ from "../components/vibeCoding/VCFAQ";
import VCFeatures from "../components/vibeCoding/VCFeatures";
import VCFinalCTA from "../components/vibeCoding/VCFinalCTA";
import VCHero from "../components/vibeCoding/VCHero";
import VCIncludedStrip from "../components/vibeCoding/VCIncludedStrip";
import VCTestimonials from "../components/vibeCoding/VCTestimonials";
import VCWhyWasp from "../components/vibeCoding/VCWhyWasp";
import VCWorkflow from "../components/vibeCoding/VCWorkflow";

import waspCoverPhoto from "../../static/img/wasp_twitter_cover.png";
import "./index.css";
import "./preflight.css";
import styles from "./styles.module.css";

const LightsTwo = () => (
  <div className="pointer-events-none absolute left-0 top-[1800px] h-full w-full overflow-hidden lg:top-[1000px]">
    <span className={classNames(styles.lightsTwo, "opacity-100")} />
  </div>
);

const Divider = () => (
  <div className="mx-auto max-w-6xl px-6">
    <div className="border-t border-neutral-200" />
  </div>
);

const VibeCoding = () => {
  const { siteConfig } = useDocusaurusContext();
  const coverPhotoAbsoluteUrl = `${siteConfig.url}${waspCoverPhoto}`;

  useBrokenLinks().collectAnchor("faq");
  useBrokenLinks().collectAnchor("signup");

  return (
    <div className="twLandingPage">
      <Head>
        <title>Wasp: The Framework Built for Vibe Coding</title>
        <meta property="og:type" content="website" />
        <meta property="og:url" content="https://wasp.sh/vibe-coding" />
        <meta
          property="og:title"
          content="Wasp: The Framework Built for Vibe Coding"
        />
        <meta
          property="og:description"
          content="The full-stack framework that AI just gets, allowing your agents to focus on writing business logic, not boilerplate."
        />
        <meta property="og:image" content={coverPhotoAbsoluteUrl} />
        <meta property="twitter:card" content="summary_large_image" />
        <meta property="twitter:url" content="https://wasp.sh/vibe-coding" />
        <meta
          property="twitter:title"
          content="Wasp: The Framework Built for Vibe Coding"
        />
        <meta
          property="twitter:description"
          content="The full-stack framework that AI just gets, allowing your agents to focus on writing business logic, not boilerplate."
        />
        <meta property="twitter:image" content={coverPhotoAbsoluteUrl} />
      </Head>
      <Nav />
      <div className="min-h-screen">
        <main>
          <div className="relative">
            <div className="pointer-events-none absolute inset-0 overflow-hidden">
              <LightsTwo />
            </div>
            <div className="relative z-10">
              <VCHero />
              <VCIncludedStrip />
              <Divider />
              <VCWhyWasp />
              <Divider />
              <VCExamples />
              <Divider />
              <VCTestimonials />
              <Divider />
              <VCWorkflow />
              <Divider />
              <VCBenchmark />
              <Divider />
              <VCFeatures />
              <Divider />
              <VCCodeComparison />
              <Divider />
              <VCFAQ />
              <Divider />
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
