import React, { Component } from 'react'
import PropTypes from 'prop-types'

import Divider from '@material-ui/core/Divider'
import JsonBrowser from './JsonBrowser'

import InfoBox from './InfoBox'

export default class EntityFormInfoBox extends Component {
  static propTypes = {
    isXRayModeOn: PropTypes.bool,

    entityName: PropTypes.string,
    entity: PropTypes.object,

    listName: PropTypes.string,
    list: PropTypes.object
  }

  render() {
    const { entityName, listName, entity, list } = this.props

    const title = listName

    const popoverTitle = (
      <div>
        entity-list&lt;<strong>{entityName}</strong>&gt;
        &nbsp;<strong>{listName}</strong>
      </div>
    )

    const popoverContent = (
      <div>
        {popoverTitle}
        <Divider/>
        <br/>
        <div>entity: {entityName}</div>
        <JsonBrowser src={entity} />
        <br/>
        <div>List properties:</div>
        <JsonBrowser src={list} />
      </div>
    )

    return (
      <InfoBox
        isXRayModeOn={this.props.isXRayModeOn}
        component={this.props.children}
        title={title}
        popoverContent={popoverContent}
      />
    )
  }

}
