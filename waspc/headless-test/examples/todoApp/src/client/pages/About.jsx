import React from 'react'
import { Link } from "wasp/client/router";

const About = () => {
  return (
    <>
      <div>I am About page!</div>
      <Link to="/">Go to dashboard</Link>
    </>
  )
}

export default About
