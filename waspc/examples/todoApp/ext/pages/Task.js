import React, { Component } from 'react'

import { Link } from "react-router-dom"

export default class Task extends Component {

  render() {
    return (
      <>
        <div>
          I am showing task with id: {this.props.match.params.id}.
        </div>
        <br/>
        <div>
          {/* We have to parse query string by ourselves, e.g. using 'query-string' package. */}
          query string is: {this.props.location.search}
        </div>
        <br/>
      <Link to="/">Go to dashboard</Link>
      </>
    )
  }
}
