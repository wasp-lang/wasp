import React from 'react'
import WaspSourceHeader from './WaspSourceHeader'

const addWaspSourceHeader = (Component) => {
  return function AddHeader(props) {
    return (
      <>
        <WaspSourceHeader name="Waspleau"/>
        <Component { ...props } />
      </>
    )
  }
}

export default addWaspSourceHeader
