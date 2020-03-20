import React, { Component } from 'react'
import PropTypes from 'prop-types'

import Divider from '@material-ui/core/Divider'
import JsonBrowser from './JsonBrowser'

import InfoBox from './InfoBox'

export default class EntityFormInfoBox extends Component {
  static propTypes = {
    entityName: PropTypes.string,
    formName: PropTypes.string,

    entity: PropTypes.object,
    fields: PropTypes.object,
  }
  
  render() {
    const { entityName, formName, fields, entity } = this.props

    const title = formName

    const popoverTitle = (
      <div>
        entity-form&lt;<strong>{entityName}</strong>&gt;
        &nbsp;<strong>{formName}</strong>
      </div>
    )

    const popoverContent = (
      <div>
        {popoverTitle}
        <Divider/>
        <br/>
        <div>
          This is a <strong>form</strong> component that creates a
          new <strong>{entityName}</strong>.
        </div>
        <br/>
        <div>Entity <strong>{entityName}</strong>:</div>
        <JsonBrowser src={entity} />
        <br/>
        <div><strong>{formName}</strong> configuration:</div>
        <JsonBrowser src={fields} />
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
