import Head from 'next/head'
import Image from 'next/image'
import classNames from 'classnames'

import Nav from 'components/Nav/index'
import Hero from 'components/Hero'
import Benefits from 'components/Benefits'
import HowItWorks from 'components/HowItWorks'
import ShowcaseGallery from 'components/ShowcaseGallery'
import Faq from 'components/Faq'
import Footer from 'components/Footer'

import styles from '../styles/index.module.css'


const Background = () => {
  return (
    <div className='absolute top-0 left-0 w-full h-full overflow-hidden pointer-events-none'>
      <span className={classNames(styles.leftLights, "opacity-100")} />
    </div>
  )
}

const LightsTwo = () => (
  <div className='absolute top-[1800px] lg:top-[1000px] left-0 w-full h-full overflow-hidden pointer-events-none'>
    <span className={classNames(styles.lightsTwo, "opacity-100")} />
  </div>
)

const Index = () => {
  return (
    <>
      <Nav />
      <div className='min-h-screen'>
        <main>
          <Background />
          <div> {/* container */}

            <Hero />
            <Benefits />

            <LightsTwo />

            <HowItWorks />
            <ShowcaseGallery />
            <Faq />

          </div> {/* eof container */}
        </main>
      </div>
      <Footer />
    </>

  )
}

export default Index
