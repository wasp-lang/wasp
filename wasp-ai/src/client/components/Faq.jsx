import React, { useState } from 'react'
import { Link } from 'react-router-dom'
import { FiChevronDown, FiChevronRight } from 'react-icons/fi'

const faqs = [
  {
    question: 'What is GPT Web App Generator?',
    answer: <p>
      <strong>TL;DR</strong> - GPT Web App Generator scaffolds a complete full-stack application based on your description.
      <br/><br/>
      Paragraph 2
      <br/><br/>
      Paragraph 3
    </p>
  },
  {
    question: 'What is Wasp?',
    answer: <p>
      Wasp is an open-source, full-stack framework for React & Node.js. It covers everything from front-end and back-end to deployment.
      You can think of it as a modern version of Ruby on Rails - opinionated, provides best practices and helps you move faster.
      Learn more about it&nbsp;
      <a 
        href='https://wasp-lang.dev/'
        className='underline decoration-2 decoration-yellow-500 font-medium'
      >here</a>
    </p>
  },
  {
    question: 'Does it always work flawlessly?',
    answer: <p>
      Since this is a GPT-powered solution, it does not provide a 100% deterministic output. That also means that, from time to time, it will introduce small mistakes in the generated code.
    </p>
  },
  {
    question: 'How complex applications can it produce?',
    answer: <p>
      Take a look at our examples.
    </p>
  },
  {
    question: 'Which stack is the code generated in?',
    answer: <p>
      React, Node.js, Prisma, and Wasp.
    </p>
  },
]

function FaqItem({ keyP, faq }) {
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
              <FiChevronDown size={20} />
            ) : (
              <FiChevronRight size={20} />
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

export function Faq() {
  return (
    <>
      <div className='grid grid-cols-12' id='faq'>
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


    </>
  )
}