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
import ClickAwayListener from '@material-ui/core/ClickAwayListener'
import Select from '@material-ui/core/Select'
import MenuItem from '@material-ui/core/MenuItem'

import * as taskState from '../state'
import * as taskActions from '../actions'

import Task from '../Task'


export class TaskList extends React.Component {
  static propTypes = {
    editable: PropTypes.bool
  }

  state = {
    taskBeingEdited: null,

    filterName: 'all'
  }

  setAsBeingEdited = task => this.setState({
    taskBeingEdited: task.id
  })

  isBeingEdited = task =>
    task.id === this.state.taskBeingEdited

  finishEditing = task => {
    if (task.id === this.state.taskBeingEdited)
      this.setState({ taskBeingEdited: null })
  }

  renderTaskDescription =
    (task) => task.isDone ? <s>{task.description}</s> : task.description

  handleFilterChange = event => {
    this.setState({ filterName: event.target.value })
  }

  filters = {
    'completed': task => task.isDone,
    'active': task => !task.isDone,
  }

  render() {
    const taskListToShow = this.state.filterName !== 'all' ?
      this.props.taskList.filter(this.filters[this.state.filterName]) :
      this.props.taskList


    return (
      <div className={this.props.className}>

        Filter:&nbsp;
        <Select
          value={this.state.filterName}
          onChange={this.handleFilterChange}
        >
          <MenuItem value="all">all</MenuItem> 
          <MenuItem value="completed">completed</MenuItem> 
          <MenuItem value="active">active</MenuItem> 
        </Select>

        <Paper>
          <Table>
            <TableHead style={{display: 'none'}}>
              <TableRow>
                  <TableCell width="50%">isDone</TableCell>
                  <TableCell width="50%">description</TableCell>
              </TableRow>
            </TableHead>

            <TableBody>
              {taskListToShow.map((task) => (
                <TableRow key={task.id}>
                    <TableCell>
                      <Checkbox
                        checked={task.isDone}
                        color="default"
                        inputProps={{
                          'aria-label': 'checkbox'
                        }}
                        disabled={!this.props.editable}
                        onChange={e => this.props.updateTask(
                          task.id, { 'isDone': e.target.checked }
                        )}
                      />
                    </TableCell>
                    <ClickAwayListener
                        onClickAway={() => this.finishEditing(task) }>
                      <TableCell
                        onDoubleClick={() => this.setAsBeingEdited(task)}
                      >
                        {this.props.editable && this.isBeingEdited(task) ? (
                          <TextField
                            value={task.description}
                            onChange={e => this.props.updateTask(
                              task.id, { 'description': e.target.value }
                            )}
                          />
                        ) : (
                          this.renderTaskDescription(task)
                        )}
                      </TableCell>
                    </ClickAwayListener>
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
  taskList: taskState.selectors.all(state)
}), {
  // Actions
  updateTask: taskActions.update
})(TaskList)
