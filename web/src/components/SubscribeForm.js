import React, { useState } from 'react'
import classNames from 'classnames'

const createNewEmailSubscriberApiEndpoint =
  'https://app.loops.so/api/newsletter-form/clg0zndc9000ajn0f8a1bhgmu'

const SubscribeForm = ({ className, inputBgColor }) => {
  const [email, setEmail] = useState('')
  const [message, setMessage] = useState('')

  const handleSubmit = async (event) => {
    // NOTE(matija): without this, the whole page reloads on form submission.
    event.preventDefault()

    try {
      const res = await fetch(createNewEmailSubscriberApiEndpoint, {
        method: 'POST',
        body: 'userGroup=&email=' + email,
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded',
        },
      })
      setMessage('Thank you for subscribing! 🙏')
    } catch (error) {
      setMessage('🛑 Oops! Something went wrong. Please try again.')
    }
  }

  return (
    <>
      {message ? (
        <p className="text-lg text-neutral-500">{message}</p>
      ) : (
        <form
          onSubmit={handleSubmit}
          className={classNames('sm:flex', className)}
        >
          <input
            aria-label="Email address"
            type="email"
            name="email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            id="email-address"
            required
            autoComplete="email"
            placeholder="you@awesomedev.com"
            className={
              `
              w-full appearance-none
              rounded-md
              border
              border-yellow-500 px-4
              py-2 text-sm placeholder:text-neutral-400
              focus:outline-none focus:ring-2 focus:ring-yellow-400
            ` + ` ${inputBgColor}`
            }
          />
          <div className="mt-3 rounded-md sm:ml-3 sm:mt-0">
            <button
              type="submit"
              className={`
                w-full
                rounded-md border border-transparent bg-yellow-500
                px-4 py-2
                text-sm text-white
                transition
                duration-200 ease-out hover:bg-yellow-400
              `}
            >
              Subscribe
            </button>
          </div>
        </form>
      )}
    </>
  )
}

export default SubscribeForm
