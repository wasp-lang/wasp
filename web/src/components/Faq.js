import React, { useState } from 'react'
import Link from '@docusaurus/Link'
import { ChevronDown, ChevronRight } from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

const faqs = [
  {
    question: 'How is Wasp different from Next.js / Nuxt.js / Gatsby?',
    answer: <p>
      <strong>TL;DR</strong> - These are frontend-first frameworks, with some limited backend capabilities.
      Wasp is a full-stack framework.
      <br/><br/>
      The main difference between Wasp and the solutions listed above is that Wasp is a trully full-stack
      framework, meaning it brings both back-end and database next to front-end. You can think of it as
      Ruby on Rails, but made for JS/TS (React & Node.js) and full-stack.
      <br/><br/>
      Next.js, Gatsby and others started out as frontend frameworks for static sites. Although some of them
      now offer an option to use serverless functions, you still have to bring your own database and you'll
      also need some kind of a server/backend if you'll need to run more complex operations.
    </p>
  },
  {
    question: 'How is Wasp different from Ruby on Rails (RoR), Django, etc?',
    answer: <p>
      While RoR and Django are full-stack frameworks, they are embedded into their host languages (ruby, python).
      However, there are degrees to "full-stackiness", and we could say that they were more full-stack in the past,
      when most of the logic for web apps was present on the server.
      These days, with advent of SPAs and frontend frameworks as React, much of that logic moved to
      the client (browser), therefore in Javascript, which makes frameworks like RoR and Django less
      full-stackish, as their integration with it is not as natural for them.

      On the other hand, Wasp was made for JS specifically, plus it is not embedded in a specific host language,
      not even JS, but has additonal pre-processing step that makes it more detached from specific language,
      laying grounds for spanning multiple languages and technologies in the future.
    </p>
  },
  {
    question: 'How hard is it to learn Wasp?',
    answer: <p>
      We measured! <strong>It takes about 30 minutes to get going</strong>, and
      most users find it pretty straight-forward.
      Since the majority of your coding will still be done in the tools you're familiar with (currently
      React & Node.js), it's really a marginal change to what you're used to.
      <br/><br/>
      The reason is for that is that Wasp is a really simple configuration language, without any
      loops or variables - you can think of it as of a JSON that is easier to read and is a bit smarter.
      <br/><br/>
      Still, although simple (and we plan to keep it that way), it's a real language so you get all the
      IDE goodies with it - syntax highlighting, auto-completion, live error reporting, ...
    </p>
  },
  {
    question: 'Do you support only React & Node.js currently?',
    answer:<p>
      Yes, that is currently the supported stack. But, Wasp is being developed as a language/framework and
      architecture-agnostic tool, so we plan to add support for more languages and frameworks in the future.
      <br/><br/>
      This is something we're pretty excited about and think could be potentially be a unique opportunity 
      due to the language approach we're taking with Wasp.
    </p>
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

    </SectionContainer>
  )
}

export default Faq
