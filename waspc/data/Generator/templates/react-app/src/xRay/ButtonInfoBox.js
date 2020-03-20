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
    buttonName: PropTypes.string,
    entityName: PropTypes.string,
    actionName: PropTypes.string,
    actionCode: PropTypes.string
  }

  prettifyCode = src => prettier.format(src, {
      parser: "babel",
      plugins: [parserBabylon],
      semi: false
    }
  )

  doesButtonHaveAction = this.props.actionName && this.props.actionCode && this.props.entityName

  render() {
    const { buttonName, entityName, actionName, actionCode } = this.props

    const title = buttonName

    const popoverTitle = (
      <div>
        button <strong>{buttonName}</strong>
      </div>
    )

    const popoverContent = (
      <div>
        {popoverTitle}
        <Divider />
        <br/>
        <span>This is a <strong>button</strong> component.</span>

        { this.doesButtonHaveAction && (
            <>
              &nbsp;<span>
                On click, action on <strong>Tasks</strong> is executed.
              </span>
              <br/><br/>

              <div>
                action&lt;<strong>{entityName}</strong>&gt; 
                &nbsp;<strong>{actionName}</strong>
                <br/>
                (takes a list of all {entityName}s and returns a new, modified list):
              </div>
              <SyntaxHighlighter
                language="javascript"
                style={docco}
              >
                {this.prettifyCode(actionCode)}
              </SyntaxHighlighter>

              <div>
                The function above was defined by the user in a <code>.wasp</code> file.
                Because Wasp "understands"<br/>what a web app is, we can <strong>inspect
                this function here
                and potentially even edit it</strong>.
              </div>
            </>
          )
        }

      </div>
    )

    return (
      <InfoBox
        component={this.props.children}
        title={title}
        popoverContent={popoverContent}
      />
    )
  }

}
