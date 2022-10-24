import Head from 'next/head'
import Image from 'next/image'
import styles from '../styles/Home.module.css'

import Hero from 'components/Hero'

const Index = () => {
  return (
    <>
      {/* navbar goes here */}
      <div className='min-h-screen'>
          <main>
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
