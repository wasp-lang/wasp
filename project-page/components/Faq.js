import { ChevronDown } from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

const faqs = [
  {
    question: 'How is Wasp different from Next.js?',
    answer: 'Next.js is front-end solution only, focused on static websites.'
  },
  {
    question: 'How is Wasp different from Ruby on Rails, Django, etc?',
    answer: 'Those are all back-end frameworks. Wasp is full-stack!'
  },
  {
    question: 'How hard is it to learn Wasp?',
    answer: 'Well, it is actually really easy!'
  }
]

const FaqItem = ({ key, faq }) => {
  
  return (
    <dt key={key} className='py-6 text-base text-neutral-700'>
      <button className='text-left w-full flex items-center justify-between'>
        <span>{faq.question}</span>
        <ChevronDown size={20} className='ml-6 text-yellow-500' />
      </button>
    </dt>
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
              className='underline decoration-2 decoration-yellow-500'
            >
              our Discord
            </a>!
          </p>
        </div>
      </div>

      <dl className='mt-6 max-w-3xl mx-auto divide-y divide-neutral-300'>
        {faqs.map((faq, idx) => (
          <FaqItem key={idx} faq={faq} />
        ))}
      </dl>

    </SectionContainer>
  )
}

export default Faq
