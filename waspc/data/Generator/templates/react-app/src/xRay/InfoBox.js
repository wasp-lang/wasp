import React, { Component } from 'react'
import PropTypes from 'prop-types'

import { withStyles } from '@material-ui/core/styles'
import IconButton from '@material-ui/core/IconButton'
import InfoIcon from '@material-ui/icons/Info'
import Popover from '@material-ui/core/Popover'
import Typography from '@material-ui/core/Typography'

import './InfoBox.css'

const styles = theme => ({
  typography: {
    padding: theme.spacing(2),
  },
})

class InfoBox extends Component {
  static propTypes = {
    isXRayModeOn: PropTypes.bool,
    component: PropTypes.node,
    title: PropTypes.string,
    popoverContent: PropTypes.node
  }

  state = {
    popoverAnchorEl: null
  }

  showPopover = event => {
    this.setState({ popoverAnchorEl: event.currentTarget })
  }

  hidePopover = () => {
    this.setState({ popoverAnchorEl: null })
  }
  

  render() {
    if (!this.props.isXRayModeOn) {
      return this.props.component
    } else {
      return (
        <div>
          <span className='xRay-infoBox-text'>{this.props.title}</span>
          <IconButton onClick={this.showPopover}>
            <InfoIcon />
          </IconButton>
          <div className='xRay-infoBox'>
            { this.props.component }
          </div>
          <Popover
            id="infobox-popover"
            open={Boolean(this.state.popoverAnchorEl)}
            anchorEl={this.state.popoverAnchorEl}
            onClose={this.hidePopover}

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
              <div className='xRay-infoBox-text'>
                {this.props.popoverContent}
              </div>
            </Typography>
          </Popover>
        </div>
      )
    }
  }
}

export default withStyles(styles)(InfoBox)
