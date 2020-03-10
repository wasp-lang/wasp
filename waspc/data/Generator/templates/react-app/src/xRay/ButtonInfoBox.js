
import React, { Component } from 'react'
import PropTypes from 'prop-types'

import Divider from '@material-ui/core/Divider'
import SyntaxHighlighter from 'react-syntax-highlighter'
import { docco } from 'react-syntax-highlighter/dist/esm/styles/hljs'
import prettier from 'prettier/standalone'
import parserBabylon from 'prettier/parser-babylon'

import InfoBox from './InfoBox'

export default class ButtonInfoBox extends Component {
  static propTypes = {
    isXRayModeOn: PropTypes.bool,
    component: PropTypes.node,

    buttonName: PropTypes.string,
    entityName: PropTypes.string,
    actionName: PropTypes.string,
    actionCode: PropTypes.string
  }

  render() {
    const { buttonName, entityName, actionName, actionCode } = this.props

    const title = buttonName

    const popoverTitle = (
      <div>
        button <strong>{buttonName}</strong>
      </div>
    )

    const prettyActionCode = prettier.format(actionCode, {
        parser: "babel",
        plugins: [parserBabylon],
        semi: false
      }
    )

    const popoverContent = (
      <div>
        {popoverTitle}
        <Divider />
        <br/>
        <div>onClick → action&lt;<strong>{entityName}</strong>&gt; 
        &nbsp;<strong>{actionName}</strong>:</div>
        <SyntaxHighlighter
          language="javascript"
          style={docco}
        >
          {prettyActionCode}
        </SyntaxHighlighter>
      </div>
    )

    return (
      <InfoBox
        isXRayModeOn={this.props.isXRayModeOn}
        component={this.props.component}
        title={title}
        popoverContent={popoverContent}
      />
    )
  }

}
