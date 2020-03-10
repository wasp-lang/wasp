import React, { Component } from 'react'
import ReactJson from 'react-json-view'

// A wrapper component, puts ReactJson options to
// the default values we want to use everywhere.
const JsonBrowser = ({ src }) => (
  <ReactJson 
    src={src}
    name={null}
    displayObjectSize={false}
    displayDataTypes={false}
  />
)

export default JsonBrowser
