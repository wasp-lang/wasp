import React, { Component } from 'react'
import { connect } from 'react-redux'

import Todo from "././ext-src/Todo"

import * as taskState from '././entities/task/state.js'
import * as taskActions from '././entities/task/actions.js'
import Task from '././entities/task/Task.js'

import NewTaskForm from '././entities/task/components/NewTaskForm.js'

import TaskList from '././entities/task/components/TaskList.js'

import '././Main.css'


export class Main extends Component {
  // TODO: Add propTypes.

  render() {
    return (
      <Todo
            addTask={this.props.addTask}
            taskList={this.props.taskList}
            updateTask={this.props.updateTask}
            removeTask={this.props.removeTask}
        >
        </Todo>
    )
  }
}

export default connect(state => ({
  taskList: taskState.selectors.all(state)
}), {
  addTask: taskActions.add,
  updateTask: taskActions.update,
  removeTask: taskActions.remove
})(Main)
