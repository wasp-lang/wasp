import Head from 'next/head'
import Image from 'next/image'
import classNames from 'classnames'

import Nav from 'components/Nav/index'
import Hero from 'components/Hero'
import Benefits from 'components/Benefits'
import HowItWorks from 'components/HowItWorks'

import styles from '../styles/index.module.css'


const Background = () => {
  return (
    <div className='absolute top-0 left-0 w-full h-full overflow-hidden pointer-events-none'>
      <span className={classNames(styles.leftLights, "opacity-100")} />
    </div>
  )
}

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
            <HowItWorks />

          </div> {/* eof container */}
        </main>
      </div>
      {/* footer goes here */}
    </>

  )
}

export default Index
