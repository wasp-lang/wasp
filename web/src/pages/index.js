import React from 'react'
import classNames from 'classnames'

import Head from '@docusaurus/Head'
import Nav from '../components/Nav/index'
import Hero from '../components/Hero'
import Features from '../components/Features'
import Roadmap from '../components/Roadmap'
import Benefits from '../components/Benefits'
import Testimonials from '../components/Testimonials'
import ExampleWaspApps from '../components/ExampleWaspApps'
import HowItWorks from '../components/HowItWorks'
import VideoAndTutorial from '../components/VideoAndTutorial'
import ShowcaseGallery from '../components/ShowcaseGallery'
import Newsletter from '../components/Newsletter'
import Faq from '../components/Faq'
import Footer from '../components/Footer'

import waspCoverPhoto from '../../static/img/wasp_twitter_cover.png'
import styles from './styles.module.css'
import './index.css'
import './preflight.css'

import useDocusaurusContext from '@docusaurus/useDocusaurusContext'

const Background = () => {
  return (
    <div className="pointer-events-none absolute left-0 top-0 h-full w-full overflow-hidden">
      <span className={classNames(styles.leftLights, 'opacity-100')} />
    </div>
  )
}

const LightsTwo = () => (
  <div className="pointer-events-none absolute left-0 top-[1800px] h-full w-full overflow-hidden lg:top-[1000px]">
    <span className={classNames(styles.lightsTwo, 'opacity-100')} />
  </div>
)

const Index = () => {
  const { siteConfig } = useDocusaurusContext()
  const coverPhotoAbsoluteUrl = `${siteConfig.url}${waspCoverPhoto}`
  return (
    <div className="twLandingPage">
      <Head>
        {/* opengraph / facebook */}
        <meta property="og:type" content="website" />
        <meta property="og:url" content="https://wasp-lang.dev/" />
        <meta
          property="og:description"
          content="Develop full-stack web apps without boilerplate."
        />
        <meta property="og:image" content={coverPhotoAbsoluteUrl} />
        {/* twitter */}
        <meta property="twitter:card" content="summary_large_image" />
        <meta property="twitter:url" content="https://wasp-lang.dev/" />
        <meta
          property="twitter:title"
          content="Develop full-stack web apps without boilerplate."
        />
        <meta property="twitter:image" content={coverPhotoAbsoluteUrl} />
      </Head>
      <Nav />
      <div className="min-h-screen">
        <main>
          <Background />
          <div>
            {/* container */}
            <Hero />
            <Features />
            <HowItWorks />
            <ExampleWaspApps />
            <Testimonials />
            <LightsTwo />
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
  )
}

export default Index
