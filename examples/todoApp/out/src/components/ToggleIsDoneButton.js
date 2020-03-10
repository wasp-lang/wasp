import React from 'react'
import { connect } from 'react-redux'
import Button from '@material-ui/core/Button'

import { toggleIsDoneAction } from '../entities/task/actions.js'

export class ToggleIsDoneButton extends React.Component {
  // TODO: Add propTypes.

  onClick = () => {
      this.props.toggleIsDoneAction()
  }

  render() {
    return (
      <Button {...this.props}
        onClick={this.onClick}
      >
        ✓
      </Button>
    )
  }
}

export default connect(state => ({
  // Selectors
}), {
  // Actions
  toggleIsDoneAction
})(ToggleIsDoneButton)
