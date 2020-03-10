{{={= =}=}}
import React, { Component } from 'react'
import { connect } from 'react-redux'

{=# wasp.isXRayModeEnabled =}
import Switch from '@material-ui/core/Switch'

import * as xRayState from './xRay/state.js'
import * as xRayActions from './xRay/actions.js'
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
        <Switch
          checked={this.props.isXRayModeOn}
          onChange={e => this.props.setXRayMode(e.target.checked)}
        />
        <span>X-Ray</span>
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
{=# wasp.isXRayModeEnabled =}
  isXRayModeOn: xRayState.selectors.isXRayModeOn(state)
{=/ wasp.isXRayModeEnabled =}
}), {
{=# entities =}
  add{= entityUpperName =}: {= entityLowerName =}Actions.add,
  update{= entityUpperName =}: {= entityLowerName =}Actions.update,
  remove{= entityUpperName =}: {= entityLowerName =}Actions.remove,
{=/ entities =}
{=# wasp.isXRayModeEnabled =}
  setXRayMode: xRayActions.set
{=/ wasp.isXRayModeEnabled =}
})({= page.name =})
