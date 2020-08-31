import React, { Component } from 'react'

import { Link } from "react-router-dom"

export default class About extends Component {

  render() {
    return (
      <>
        <div>I am About page!</div>
        <Link to="/">Go to dashboard</Link>
      </>
    )
  }
}
