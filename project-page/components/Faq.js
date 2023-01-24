import { useState } from 'react'
import Link from 'next/link'
import { ChevronDown, ChevronRight } from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

const faqs = [
  {
    question: 'How is Wasp different from Next.js / Nuxt.js / Gatsby?',
    answer: <p>
      Next.js is front-end solution only, focused on static websites. This is some
      longer text so I can see how it behaves.<br/>

      Maybe I should put here component so I have freedom in formatting, putting URLs, etc?
      This is some test <Link href='/'><a>url</a></Link>
    </p>
  },
  {
    question: 'How is Wasp different from Ruby on Rails, Django, etc?',
    answer: 'Those are all back-end frameworks. Wasp is full-stack!'
  },
  {
    question: 'How hard is it to learn Wasp?',
    answer: 'Well, it is actually really easy!'
  },
  {
    question: 'Do you support only React currently?',
    answer: 'Well, it is actually really easy!'
  }
]

const FaqItem = ({ keyP, faq }) => {

  const [isExpanded, setIsExpanded] = useState(false)
  
  return (
    <div className='py-6'>
      <dt key={keyP} className='text-base text-neutral-700'>
        <button
          className='text-left w-full flex items-center justify-between'
          onClick={() => { setIsExpanded(!isExpanded) }}
        >
          <span>{faq.question}</span>
          <div className='ml-6 text-yellow-500'>
            {isExpanded ? (
              <ChevronDown size={20} />
            ) : (
              <ChevronRight size={20} />
            )}
          </div>
        </button>
      </dt>
      {isExpanded && (
        <dd className='mt-2 text-neutral-500'>
          {faq.answer}
        </dd>
      )}
    </div>
  )
}

const Faq = () => {
  return (
    <SectionContainer>
      <div className='grid grid-cols-12'>
        <div className='col-span-12 text-center'>
          <h2 className='text-xl lg:text-2xl text-neutral-700 mb-4'>
            Frequently asked questions
          </h2>
          <p className='text-neutral-500'>
            For anything not covered here, join&nbsp;
            <a 
              href='https://discord.gg/rzdnErX'
              className='underline decoration-2 decoration-yellow-500 font-medium'
            >
              our Discord
            </a>!
          </p>
        </div>
      </div>

      <dl className='mt-6 max-w-3xl mx-auto divide-y divide-neutral-300'>
        {faqs.map((faq, idx) => (
          <FaqItem keyP={idx} key={idx} faq={faq} />
        ))}
      </dl>

    </SectionContainer>
  )
}

export default Faq
