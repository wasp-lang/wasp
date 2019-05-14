{{={= =}=}}
import React, { Component } from 'react'
import { connect } from 'react-redux'

{=# entities =}
import * as {= entityLowerName =}State from '{= entityStatePath =}'
import * as {= entityLowerName =}Actions from '{= entityActionsPath =}'
import {= entity.name =} from '{= entityClassPath =}'
{=/ entities =}


export class {= page.name =} extends Component {
  // TODO: Add propTypes.

  render() {
    return (
      {=& page.content =}
    )
  }
}

export default connect(state => ({
{=# entities =}
  {= entityLowerName =}List: {= entityLowerName =}State.selectors.all(state)
{=/ entities =}
}), {
{=# entities =}
  add{= entityUpperName =}: {= entityLowerName =}Actions.add
{=/ entities =}
})({= page.name =})
