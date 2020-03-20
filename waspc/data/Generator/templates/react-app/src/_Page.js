{{={= =}=}}
import React, { Component } from 'react'
import { connect } from 'react-redux'

{=# wasp.isXRayModeEnabled =}
import XRaySwitch from './xRay/XRaySwitch'
{=/ wasp.isXRayModeEnabled =}

{=# jsImports =}
import {= what =} from "{= from =}"
{=/ jsImports =}

{=# entities =}
import * as {= entityLowerName =}State from '{= entityStatePath =}'
import * as {= entityLowerName =}Actions from '{= entityActionsPath =}'
import {= entity.name =} from '{= entityClassPath =}'
{=# entityCreateForms =}
import {= entityForm.name =} from '{= path =}'
{=/ entityCreateForms =}
{=# entityLists =}
import {= entityList.name =} from '{= path =}'
{=/ entityLists =}
{=/ entities =}

{=# pageStylePath =}
import '{= pageStylePath =}'
{=/ pageStylePath =}


export class {= page.name =} extends Component {
  // TODO: Add propTypes.

  render() {
    return (
      {=# wasp.isXRayModeEnabled =}
      <>
      <XRaySwitch />
      {=/ wasp.isXRayModeEnabled =}
      {=& page.content =}
      {=# wasp.isXRayModeEnabled =}
      </>
      {=/ wasp.isXRayModeEnabled =}
    )
  }
}

export default connect(state => ({
{=# entities =}
  {= entityLowerName =}List: {= entityLowerName =}State.selectors.all(state),
{=/ entities =}
}), {
{=# entities =}
  add{= entityUpperName =}: {= entityLowerName =}Actions.add,
  update{= entityUpperName =}: {= entityLowerName =}Actions.update,
  remove{= entityUpperName =}: {= entityLowerName =}Actions.remove,
{=/ entities =}
})({= page.name =})
