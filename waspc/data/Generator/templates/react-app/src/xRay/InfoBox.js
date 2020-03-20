import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'

import { withStyles } from '@material-ui/core/styles'
import IconButton from '@material-ui/core/IconButton'
import InfoIcon from '@material-ui/icons/Info'
import Typography from '@material-ui/core/Typography'

import * as xRayState from './state.js'
import XRayPopover from './XRayPopover'

import './InfoBox.css'

export class InfoBox extends Component {
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
            <InfoIcon color="secondary" />
          </IconButton>
          <div className='xRay-infoBox'>
            { this.props.component }
          </div>
          <XRayPopover
            id="infobox-popover"
            open={Boolean(this.state.popoverAnchorEl)}
            anchorEl={this.state.popoverAnchorEl}
            onClose={this.hidePopover}
          >
            {this.props.popoverContent}
          </XRayPopover>
        </div>
      )
    }
  }
}

export default connect(state => ({
  // Selectors
  isXRayModeOn: xRayState.selectors.isXRayModeOn(state)
}), {
  // Actions
})(InfoBox)
