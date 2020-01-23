{{={= =}=}}
import React, { Component } from 'react'
import { connect } from 'react-redux'

{=# jsImports =}
import {= what =} from "{= from =}"
{=/ jsImports =}

{=# entities =}
import * as {= entityLowerName =}State from '{= entityStatePath =}'
import * as {= entityLowerName =}Actions from '{= entityActionsPath =}'
import {= entity.name =} from '{= entityClassPath =}'
import {= entity.name =}List from '{= entityListPath =}'

{=! Imports of entity create forms =} 
{=# entityCreateForms =}
import {= entityForm.name =} from '{= path =}'
{=/ entityCreateForms =}

{=/ entities =}
{=# pageStylePath =}
import '{= pageStylePath =}'
{=/ pageStylePath =}


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
  add{= entityUpperName =}: {= entityLowerName =}Actions.add,
  update{= entityUpperName =}: {= entityLowerName =}Actions.update,
  remove{= entityUpperName =}: {= entityLowerName =}Actions.remove
{=/ entities =}
})({= page.name =})
