import React from 'react';

import './emailSignupForm.css'

const EmailSignupForm = ({ placeholder }) => {
  return (
    <form
        className="email-signup-form"
        action="https://gmail.us4.list-manage.com/subscribe/post?u=8139c7de74df98aa17054b235&amp;id=f0c6ba5f1d"
        method="post"
    >

      <input
        aria-label="Email address"
        name="EMAIL"
        type="email"
        required
        className="input"
        placeholder={placeholder || "Enter your email"}
      />
      <div className="">
        <button className="button button--primary" type="submit">
          Subscribe
        </button>
      </div>

    </form>
  )
}

export default EmailSignupForm;
