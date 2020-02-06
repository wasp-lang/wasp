import React, { Component } from 'react'
import { connect } from 'react-redux'

import Todo from "./ext-src/Todo.js"

import * as taskState from './entities/task/state.js'
import * as taskActions from './entities/task/actions.js'
import Task from './entities/task/Task.js'
import NewTaskForm from './entities/task/components/NewTaskForm.js'
import TaskList from './entities/task/components/TaskList.js'

import './About.css'


export class About extends Component {
  // TODO: Add propTypes.

  render() {
    return (
      <div className="aboutPage">
            <h1> About </h1>
            <p>This page was built with <a href="https://wasp-lang.dev">Wasp</a>!</p>
            <p> Check out source code &nbsp;
              <a href="https://github.com/wasp-lang/wasp/tree/master/examples/todoApp">here</a>.
            </p>
        </div>
    )
  }
}

export default connect(state => ({
  taskList: taskState.selectors.all(state)
}), {
  addTask: taskActions.add,
  updateTask: taskActions.update,
  removeTask: taskActions.remove
})(About)
