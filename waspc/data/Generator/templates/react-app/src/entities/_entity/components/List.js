{{={= =}=}}
import _ from 'lodash'
import React from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'

{=# isXRayModeEnabled =}
import * as xRayState from '../../../xRay/state.js'
import EntityListInfoBox from '../../../xRay/EntityListInfoBox'
{=/ isXRayModeEnabled =}

import Paper from '@material-ui/core/Paper'
import Table from '@material-ui/core/Table'
import TableBody from '@material-ui/core/TableBody'
import TableCell from '@material-ui/core/TableCell'
import TableHead from '@material-ui/core/TableHead'
import TableRow from '@material-ui/core/TableRow'
import Checkbox from '@material-ui/core/Checkbox'
import TextField from '@material-ui/core/TextField'
import ClickAwayListener from '@material-ui/core/ClickAwayListener'
{=# mutexFiltersConfig =}
import Select from '@material-ui/core/Select'
import MenuItem from '@material-ui/core/MenuItem'
{=/ mutexFiltersConfig =}

import * as {= entityLowerName =}State from '../state'
import * as {= entityLowerName =}Actions from '../actions'

import {= entityClassName =} from '../{= entityClassName =}'


export class {= listName =} extends React.Component {
  static propTypes = {
    editable: PropTypes.bool
  }

  state = {
    {= entityBeingEditedStateVar =}: null,

    {=# mutexFiltersConfig =}
    filterName: '{= noFilterLabel =}'
    {=/ mutexFiltersConfig =}
  }

  setAsBeingEdited = {= entityLowerName =} => this.setState({
    {= entityBeingEditedStateVar =}: {= entityLowerName =}.id
  })

  isBeingEdited = {= entityLowerName =} =>
    {= entityLowerName =}.id === this.state.{= entityBeingEditedStateVar =}

  finishEditing = {= entityLowerName =} => {
    if ({= entityLowerName =}.id === this.state.{= entityBeingEditedStateVar =})
      this.setState({ {= entityBeingEditedStateVar =}: null })
  }

  {=! Render "render" functions for each field, if provided =}
  {=# listFields =}
  {=# render =}
  {= renderFnName =} =
    {=& render =}
  {=/ render =}
  {=/ listFields =}

  {=# mutexFiltersConfig =}
  handleFilterChange = event => {
    this.setState({ filterName: event.target.value })
  }

  filters = {
    {=# filters =}
    {= name =}: {=& predicate =},
    {=/ filters =}
  }
  {=/ mutexFiltersConfig =}

  render() {
  {=# mutexFiltersConfig =}
    const {= entitiesToShowRenderVar =} = this.state.filterName !== '{= noFilterLabel =}' ?
      {=! TODO(matija): duplication, we could extract entityLowerName_List =}
      this.props.{= entityLowerName =}List.filter(this.filters[this.state.filterName]) :
      this.props.{= entityLowerName =}List
  {=/ mutexFiltersConfig =}

  {=^ mutexFiltersConfig =}
    const {= entitiesToShowRenderVar =} = this.props.{= entityLowerName =}List
  {=/ mutexFiltersConfig =}

    {=# isXRayModeEnabled =}
    const entity = {
      {=# listFields =}
        {= name =}: '{= type =}',
      {=/ listFields =}
    }

    const list = {
      {=# showHeader =}
      showHeader: true,
      {=/ showHeader =}
      {=^ showHeader =}
      showHeader: false,
      {=/ showHeader =}

      {=# mutexFiltersConfig =}
      mutuallyExclusiveFilters: {
        {=# filters =}
        {= name =}: '{=& predicate =}',
        {=/ filters =}
      },
      {=/ mutexFiltersConfig =}

      fields: {
        {=# listFields =}
        {= name =}: {
          {=# render =}
            render: '{=& render =}',
          {=/ render =}
        },
        {=/ listFields =}
      }
    }

    {=/ isXRayModeEnabled =}

    return (
      <div className={this.props.className}>
      {=# isXRayModeEnabled =}
      <EntityListInfoBox
        isXRayModeOn={this.props.isXRayModeOn}

        entityName="{= entityClassName =}"
        entity={entity}

        listName="{= listName =}"
        list={list}
      >
        <>
      {=/ isXRayModeEnabled =}

        {=# mutexFiltersConfig =}
        Filter:&nbsp;
        <Select
          value={this.state.filterName}
          onChange={this.handleFilterChange}
        >
          <MenuItem value="{= noFilterLabel =}">{= noFilterLabel =}</MenuItem> 
          {=# filters =}
          <MenuItem value="{= name =}">{= name =}</MenuItem> 
          {=/ filters =}
        </Select>
        {=/ mutexFiltersConfig =}

        <Paper>
          <Table>
            <TableHead{=^ showHeader =} style={{display: 'none'}}{=/ showHeader =}>
              <TableRow>
                {=# listFields =}
                  <TableCell width="{= widthAsPercent =}%">{= name =}</TableCell>
                {=/ listFields =}
              </TableRow>
            </TableHead>

            <TableBody>
              {{= entitiesToShowRenderVar =}.map(({= entityLowerName =}) => (
                <TableRow key={{= entityLowerName =}.id}>
                  {=# listFields =}
                  {=# boolean =}
                    <TableCell>
                      <Checkbox
                        checked={{= entityLowerName =}.{= name =}}
                        color="default"
                        inputProps={{
                          'aria-label': 'checkbox'
                        }}
                        disabled={!this.props.editable}
                        onChange={e => this.props.update{= entityName =}(
                          {= entityLowerName =}.id, { '{= name =}': e.target.checked }
                        )}
                      />
                    </TableCell>
                  {=/ boolean =}
                  {=# string =}
                    <ClickAwayListener
                        onClickAway={() => this.finishEditing({= entityLowerName =}) }>
                      <TableCell
                        onDoubleClick={() => this.setAsBeingEdited({= entityLowerName =})}
                      >
                        {this.props.editable && this.isBeingEdited({= entityLowerName =}) ? (
                          <TextField
                            value={{= entityLowerName =}.{= name =}}
                            onChange={e => this.props.update{= entityName =}(
                              {= entityLowerName =}.id, { '{= name =}': e.target.value }
                            )}
                          />
                        ) : (
                          {=# render =}
                          this.{= renderFnName =}({= entityLowerName =})
                          {=/ render =}
                          {=^ render =}
                          {= entityLowerName =}.{= name =}
                          {=/ render =}
                        )}
                      </TableCell>
                    </ClickAwayListener>
                  {=/ string =}
                  {=/ listFields =}
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </Paper>
        {=# isXRayModeEnabled =}
          </>
        </EntityListInfoBox>
        {=/ isXRayModeEnabled =}
      </div>
    )
  }
}

export default connect(state => ({
  // Selectors
  {= entityLowerName =}List: {= entityLowerName =}State.selectors.all(state),
  {=# isXRayModeEnabled =}
  isXRayModeOn: xRayState.selectors.isXRayModeOn(state)
  {=/ isXRayModeEnabled =}
}), {
  // Actions
  update{= entityName =}: {= entityLowerName =}Actions.update
})({= listName =})
