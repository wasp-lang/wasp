import Head from "@docusaurus/Head";
import useBrokenLinks from "@docusaurus/useBrokenLinks";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";

import Footer from "../components/Footer";
import Nav from "../components/Nav/index";
import VCHero from "../components/vibeCoding/VCHero";
import VCWhyWasp from "../components/vibeCoding/VCWhyWasp";
import VCBenchmark from "../components/vibeCoding/VCBenchmark";
import VCTestimonials from "../components/vibeCoding/VCTestimonials";
import VCWorkflow from "../components/vibeCoding/VCWorkflow";
import VCExamples from "../components/vibeCoding/VCExamples";
import VCFeatures from "../components/vibeCoding/VCFeatures";
import VCCodeComparison from "../components/vibeCoding/VCCodeComparison";
import VCFAQ from "../components/vibeCoding/VCFAQ";
import VCFinalCTA from "../components/vibeCoding/VCFinalCTA";

import waspCoverPhoto from "../../static/img/wasp_twitter_cover.png";
import classNames from "classnames";

import "./index.css";
import "./preflight.css";
import styles from "./styles.module.css";

const LightsTwo = () => (
  <div className="pointer-events-none absolute left-0 top-[1800px] h-full w-full overflow-hidden lg:top-[1000px]">
    <span className={classNames(styles.lightsTwo, "opacity-100")} />
  </div>
);

const Divider = () => (
  <div className="mx-auto max-w-6xl px-6"><div className="border-t border-neutral-200" /></div>
);

const V = "v3";

const VibeCodingV3 = () => {
  const { siteConfig } = useDocusaurusContext();
  const coverPhotoAbsoluteUrl = `${siteConfig.url}${waspCoverPhoto}`;

  useBrokenLinks().collectAnchor("faq");
  useBrokenLinks().collectAnchor("signup");

  return (
    <div className="twLandingPage">
      <Head>
        <title>Wasp LP — V3 (Bold)</title>
      </Head>
      <Nav />
      <div className="min-h-screen">
        <main>
          <div className="relative">
            <div className="pointer-events-none absolute inset-0 overflow-hidden">
              <LightsTwo />
            </div>
            <div className="relative z-10">
              <VCHero variant={V} />
              <Divider />
              <VCWhyWasp variant={V} />
              <Divider />
              <VCBenchmark variant={V} />
              <Divider />
              <VCExamples variant={V} />
              <Divider />
              <VCTestimonials variant={V} />
              <Divider />
              <VCFeatures variant={V} />
              <Divider />
              <VCCodeComparison variant={V} />
              <Divider />
              <VCWorkflow variant={V} />
              <Divider />
              <VCFAQ variant={V} />
              <Divider />
              <VCFinalCTA variant={V} />
            </div>
          </div>
        </main>
      </div>
      <Footer />
    </div>
  );
};

export default VibeCodingV3;
