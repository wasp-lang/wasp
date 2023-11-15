import React from 'react'
import WaspSourceHeader from './WaspSourceHeader'

const addWaspSourceHeader = (Component) => {
  return function AddHeader(props) {
    return (
      <>
        <WaspSourceHeader name="Thoughts" />
        <Component { ...props }/>
      </>
    )
  }
}

export default addWaspSourceHeader
