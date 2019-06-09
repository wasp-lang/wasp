{{={= =}=}}
import _ from 'lodash'
import React from 'react'
import { connect } from 'react-redux'

import Paper from '@material-ui/core/Paper'
import Table from '@material-ui/core/Table'
import TableBody from '@material-ui/core/TableBody'
import TableCell from '@material-ui/core/TableCell'
import TableHead from '@material-ui/core/TableHead'
import TableRow from '@material-ui/core/TableRow'

import * as {= entityLowerName =}State from '../state'

import {= entityClassName =} from '../{= entityClassName =}'


export class List extends React.Component {

  render() {
    return (
      <div style={ { margin: '20px' } }>
        <Paper>
          <Table>
            <TableHead>
              <TableRow>
                {=# entityTypedFields =}
                {=# boolean =}
                  <TableCell>{= name =} (bool)</TableCell>
                {=/ boolean =}
                {=# string =}
                  <TableCell>{= name =} (string)</TableCell>
                {=/ string =}
                {=/ entityTypedFields =}
              </TableRow>
            </TableHead>

            <TableBody>
              {this.props.{= entityLowerName =}List.map({= entityLowerName =} => (
                <TableRow>
                  {=# entityTypedFields =}
                  {=# boolean =}
                    <TableCell>
                      {{= entityLowerName =}.{= name =} || 'no bool value'}
                    </TableCell>
                  {=/ boolean =}
                  {=# string =}
                    <TableCell>
                      {{= entityLowerName =}.{= name =} || 'no string value'}
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
})(List)
