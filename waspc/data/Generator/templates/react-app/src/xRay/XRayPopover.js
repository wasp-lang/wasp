import React, { Component } from 'react'
import PropTypes from 'prop-types'

import { withStyles } from '@material-ui/core/styles'
import Popover from '@material-ui/core/Popover'
import Typography from '@material-ui/core/Typography'

import './XRayPopover.css'

const styles = theme => ({
  typography: {
    padding: theme.spacing(2),
  },
  /*
  popover: {
    maxWidth: 600
  }
  */
})

class XRayPopover extends Component {
  static propTypes = {
    id: PropTypes.string,

    anchorEl: PropTypes.node,
    open: PropTypes.bool,
    onClose: PropTypes.func
  }

  render() {
    return (
      <Popover
        className={this.props.classes.popover}

        id={this.props.id}
        open={this.props.open}
        anchorEl={this.props.anchorEl}
        onClose={this.props.onClose}

        anchorOrigin={{
          vertical: 'bottom',
          horizontal: 'center',
        }}
        transformOrigin={{
          vertical: 'top',
          horizontal: 'center',
        }}
      >
        <Typography className={this.props.classes.typography}>
          <div className='xRay-popover-text'>
            {this.props.children}
          </div>
        </Typography>
      </Popover>
    )
  }
}

export default withStyles(styles)(XRayPopover)
