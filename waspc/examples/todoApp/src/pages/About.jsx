import { Link } from "wasp/client/router";

import React from 'react'

const About = () => {
  return (
    <>
      <div>I am About page!</div>
      <Link to="/">Go to dashboard</Link>
    </>
  )
}

export default About
