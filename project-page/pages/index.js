import Head from 'next/head'
import Image from 'next/image'
//import styles from '../styles/Home.module.css'
import styles from '../styles/index.module.css'

import classNames from 'classnames'

import Hero from 'components/Hero'

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
      {/* navbar goes here */}
      <div className='min-h-screen'>
        <main>
          <Background />
          <div> {/* container */}

            <Hero />

          </div> {/* eof container */}
        </main>
      </div>
      {/* footer goes here */}
    </>

  )
}

export default Index
