{{={= =}=}}
import React from 'react'
import { connect } from 'react-redux'
import Button from '@material-ui/core/Button'

{=# wasp.isXRayModeEnabled =}
import ButtonInfoBox from '../xRay/ButtonInfoBox'
{=/ wasp.isXRayModeEnabled =}

{=# onClickAction =}
import { {= exportedIdentifier =} } from '{= importPath =}'
{=/ onClickAction =}

export class {= button.name =} extends React.Component {
  // TODO: Add propTypes.

  {=# onClickAction =}
  onClick = () => {
      this.props.{= exportedIdentifier =}()
  }
  {=/ onClickAction =}

  render() {
    return (
    {=# wasp.isXRayModeEnabled =}
      <ButtonInfoBox
        buttonName="{= button.name =}"

        {=# onClickAction =}
        entityName="{= onClickAction.entityName =}"
        actionName="{= onClickAction.exportedIdentifier =}"
        actionCode={`{=& onClickAction.updateFn =}`}
        {=/ onClickAction =}
      >
      <>
    {=/ wasp.isXRayModeEnabled =}
      <Button {...this.props}
        {=# onClickAction =}
        onClick={this.onClick}
        {=/ onClickAction =}
      >
        {= button.label =}
      </Button>
    {=# wasp.isXRayModeEnabled =}
      </>
      </ButtonInfoBox>
    {=/ wasp.isXRayModeEnabled =}
    )
  }
}

export default connect(state => ({
  // Selectors
}), {
  // Actions
  {=# onClickAction =}
  {= exportedIdentifier =}
  {=/ onClickAction =}
})({= button.name =})
