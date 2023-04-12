import React from 'react'
import classNames from 'classnames'

const SubscribeForm = ({ className, inputBgColor }) => (
  <form 
    className={classNames('', className)}
    action="https://gmail.us4.list-manage.com/subscribe/post?u=8139c7de74df98aa17054b235&amp;id=f0c6ba5f1d"
    method="post"
  >
    <input
      aria-label="Email address"
      type="email"
      name="EMAIL" 
      id="email-address"
      required autoComplete='email'
      placeholder='you@areawesomeforsubscribing.com'
      className={`
        text-sm w-full
        appearance-none
        placeholder:text-neutral-400
        border border-yellow-500
        px-4 py-2 rounded-md
        focus:outline-none focus:ring-2 focus:ring-yellow-400
      ` + ` ${inputBgColor}`}
    />
    <div className='rounded-md mt-3 sm:mt-0 sm:ml-3'>
      <button
        type='submit'
        className={`
          w-full
          text-sm border border-transparent rounded-md
          bg-yellow-500 text-white
          px-4 py-2
          hover:bg-yellow-400
          transition ease-out duration-200
        `}
      >
        Subscribe
      </button>
    </div>
  </form>
)

export default SubscribeForm
