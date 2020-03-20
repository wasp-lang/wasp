import React, { Component } from 'react'
import { connect } from 'react-redux'

import Switch from '@material-ui/core/Switch'
import IconButton from '@material-ui/core/IconButton'
import InfoIcon from '@material-ui/icons/Info'

import * as xRayState from './state.js'
import * as xRayActions from './actions.js'

import XRayPopover from './XRayPopover'

export class XRaySwitch extends Component {

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
    return (
      <>
        <Switch
          checked={this.props.isXRayModeOn}
          onChange={e => this.props.setXRayMode(e.target.checked)}
        />
        <span>Wasp inspector</span>
        <IconButton onClick={this.showPopover}>
          <InfoIcon />
        </IconButton>

        <XRayPopover
          id="xRay-info-popover"
          open={Boolean(this.state.popoverAnchorEl)}
          anchorEl={this.state.popoverAnchorEl}
          onClose={this.hidePopover}
        >
          <p>
            By flipping this switch you will activate&nbsp;
            <strong>Wasp inspector</strong> mode.
          </p>

          <p>
            Since Wasp is a higher-level language it <strong>"understands" distinct parts of
            the web app</strong><br/>
            (such as <code>form</code>, <code>button</code> or <code>action</code>).
          </p>

          <p>
            We implemented this Proof-of-Concept functionality to demonstrate what that enables
            Wasp to do <br/>- it can generate additional functionalities that allow&nbsp;
            <strong>inspecting and potentially even editing of the user-defined code<br/>
            within the web-app itself</strong>.
          </p>
        </XRayPopover>
      </>
    )
  }
}

export default connect(state => ({
  isXRayModeOn: xRayState.selectors.isXRayModeOn(state)
}), {
  setXRayMode: xRayActions.set
})(XRaySwitch)
