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


export class {= listName =} extends React.Component {
  static propTypes = {
    editable: PropTypes.bool,
  }

  update{= entityName =}Field = (fieldName, newFieldValue, {= entityLowerName =}) => {
    const updated{= entityName =} = new {= entityClassName =}(
      { ...{= entityLowerName =}.toData(), [fieldName]: newFieldValue }
    )
    this.props.update{= entityName =}({= entityLowerName =}.id, updated{= entityName =})
  }
  
  render() {
    return (
      <div style={ { margin: '20px' } }>
        <Paper>
          <Table>
            <TableHead>
              <TableRow>
                {=# listFields =}
                {=# boolean =}
                  <TableCell>{= name =}</TableCell>
                {=/ boolean =}
                {=# string =}
                  <TableCell>{= name =}</TableCell>
                {=/ string =}
                {=/ listFields =}
              </TableRow>
            </TableHead>

            <TableBody>
              {this.props.{= entityLowerName =}List.map(({= entityLowerName =}) => (
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
                        onChange={e => this.update{= entityName =}Field(
                          '{= name =}', e.target.checked, {= entityLowerName =}
                        )}
                      />
                    </TableCell>
                  {=/ boolean =}
                  {=# string =}
                    <TableCell>
                      {this.props.editable ? (
                        <TextField
                          value={{= entityLowerName =}.{= name =}}
                          onChange={e => this.update{= entityName =}Field(
                            '{= name =}', e.target.value, {= entityLowerName =}
                          )}
                        />
                      ) : (
                        {= entityLowerName =}.{= name =}
                      )}
                    </TableCell>
                  {=/ string =}
                  {=/ listFields =}
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
  update{= entityName =}: {= entityLowerName =}Actions.update
})({= listName =})
