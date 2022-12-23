import React from 'react';
import classNames from 'classnames';

import Head from '@docusaurus/Head';
import Nav from '../components/Nav/index';
import Hero from '../components/Hero';
import Benefits from '../components/Benefits';
import Testimonials from '../components/Testimonials';
import ExampleWaspApps from '../components/ExampleWaspApps';
import HowItWorks from '../components/HowItWorks';
import ShowcaseGallery from '../components/ShowcaseGallery';
import Newsletter from '../components/Newsletter';
import Faq from '../components/Faq';
import Footer from '../components/Footer';

import waspLogo from '../../static/img/wasp-logo.png';
import styles from './styles.module.css';
import './index.css';
import './preflight.css';

const Background = () => {
  return (
    <div className='absolute top-0 left-0 w-full h-full overflow-hidden pointer-events-none'>
      <span className={classNames(styles.leftLights, 'opacity-100')} />
    </div>
  );
};

const LightsTwo = () => (
  <div className='absolute top-[1800px] lg:top-[1000px] left-0 w-full h-full overflow-hidden pointer-events-none'>
    <span className={classNames(styles.lightsTwo, 'opacity-100')} />
  </div>
);

const Index = () => {
  return (
    <div className='twLandingPage'>
      <Head>
        {/* opengraph / facebook */}
        <meta property='og:type' content='website' />
        <meta property='og:url' content='https://wasp-lang.dev/' />
        <meta property='og:description' content='Develop full-stack web apps without boilerplate.' />
        <meta property='og:image' content={waspLogo} />
        {/* twitter */}
        <meta property='twitter:card' content='summary_large_image' />
        <meta property='twitter:url' content='https://wasp-lang.dev/' />
        <meta property='twitter:title' content='Develop full-stack web apps without boilerplate.' />
        <meta property='twitter:image' content={waspLogo} />
      </Head>
      <Nav />
      <div className='min-h-screen'>
        <main>
          <Background />
          <div>
            {' '}
            {/* container */}
            <Hero />
            <Benefits />
            <Testimonials />
            <ExampleWaspApps />
            <LightsTwo />
            <HowItWorks />
            <ShowcaseGallery />
            <Newsletter />
            <Faq />
          </div>{' '}
          {/* eof container */}
        </main>
      </div>
      <Footer />
    </div>
  );
};

export default Index;
