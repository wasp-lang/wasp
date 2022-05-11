import React, { useEffect } from 'react'

import { Link } from 'react-router-dom'

const About = () => {

  useEffect(() => {
    console.log("Inside useEffect");
    const socket = new WebSocket("ws://localhost:8080");
    socket.onopen = function (event) {
      socket.send("Hello, world!");
    }
    socket.onmessage = function (event) {
      console.log(event.data);
    }
  })

  return (
    <>
      <div>I am About page!</div>
      <Link to='/'>Go to dashboard</Link>
    </>
  )
}

export default About;
