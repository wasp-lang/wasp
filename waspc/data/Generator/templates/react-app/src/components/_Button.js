{{={= =}=}}
import React from 'react'
import { connect } from 'react-redux'
import Button from '@material-ui/core/Button'

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
      <Button {...this.props}
        {=# onClickAction =}
        onClick={this.onClick}
        {=/ onClickAction =}
      >
        {= button.label =}
      </Button>
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
