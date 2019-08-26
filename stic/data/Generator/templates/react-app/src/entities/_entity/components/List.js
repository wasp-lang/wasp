{{={= =}=}}
import _ from 'lodash'
import React from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'

import Paper from '@material-ui/core/Paper'
import Table from '@material-ui/core/Table'
import TableBody from '@material-ui/core/TableBody'
import TableCell from '@material-ui/core/TableCell'
import TableHead from '@material-ui/core/TableHead'
import TableRow from '@material-ui/core/TableRow'
import Checkbox from '@material-ui/core/Checkbox'
import TextField from '@material-ui/core/TextField'

import * as {= entityLowerName =}State from '../state'
import * as {= entityLowerName =}Actions from '../actions'

import {= entityClassName =} from '../{= entityClassName =}'


export class List extends React.Component {
  static propTypes = {
    editable: PropTypes.bool,
  }

  update{= entity.name =}Field = (fieldName, newFieldValue, {= entityLowerName =}, idx) => {
    const updated{= entity.name =} = new {= entityClassName =}(
      { ...{= entityLowerName =}.toData(), [fieldName]: newFieldValue }
    )
    this.props.update{= entity.name =}(idx, updated{= entity.name =})
  }
  
  render() {
    return (
      <div style={ { margin: '20px' } }>
        <Paper>
          <Table>
            <TableHead>
              <TableRow>
                {=# entityTypedFields =}
                {=# boolean =}
                  <TableCell>{= name =}</TableCell>
                {=/ boolean =}
                {=# string =}
                  <TableCell>{= name =}</TableCell>
                {=/ string =}
                {=/ entityTypedFields =}
              </TableRow>
            </TableHead>

            <TableBody>
              {this.props.{= entityLowerName =}List.map(({= entityLowerName =}, idx) => (
                <TableRow key={idx}>
                  {=# entityTypedFields =}
                  {=# boolean =}
                    <TableCell>
                      <Checkbox
                        checked={{= entityLowerName =}.{= name =}}
                        color="default"
                        inputProps={{
                          'aria-label': 'checkbox'
                        }}
                        disabled={!this.props.editable}
                        onChange={e => this.update{= entity.name =}Field(
                          '{= name =}', e.target.checked, {= entityLowerName=}, idx
                        )}
                      />
                    </TableCell>
                  {=/ boolean =}
                  {=# string =}
                    <TableCell>
                      {this.props.editable ? (
                        <TextField
                          value={{= entityLowerName =}.{= name =}}
                          onChange={e => this.update{= entity.name =}Field(
                            '{= name =}', e.target.value, {= entityLowerName=}, idx
                          )}
                        />
                      ) : (
                        {= entityLowerName =}.{= name =}
                      )}
                    </TableCell>
                  {=/ string =}
                  {=/ entityTypedFields =}
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </Paper>
      </div>
    )
  }
}

export default connect(state => ({
  // Selectors
  {= entityLowerName =}List: {= entityLowerName =}State.selectors.all(state)
}), {
  // Actions
  update{= entity.name =}: {= entityLowerName =}Actions.update
})(List)
