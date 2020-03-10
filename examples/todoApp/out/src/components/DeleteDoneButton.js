import React from 'react'
import { connect } from 'react-redux'
import Button from '@material-ui/core/Button'

import { deleteDoneAction } from '../entities/task/actions.js'

export class DeleteDoneButton extends React.Component {
  // TODO: Add propTypes.

  onClick = () => {
      this.props.deleteDoneAction()
  }

  render() {
    return (
      <Button {...this.props}
        onClick={this.onClick}
      >
        Delete completed
      </Button>
    )
  }
}

export default connect(state => ({
  // Selectors
}), {
  // Actions
  deleteDoneAction
})(DeleteDoneButton)
