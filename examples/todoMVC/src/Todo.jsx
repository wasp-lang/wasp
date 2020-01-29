import React from 'react'

import Task from '@wasp/entities/task/Task'
import NewTaskForm from '@wasp/entities/task/components/NewTaskForm'
import TaskList from '@wasp/entities/task/components/TaskList'

import * as config from './config'


export default class Todo extends React.Component {
  toggleIsDoneForAllTasks = () => {
    const areAllDone = this.props.taskList.every(t => t.isDone)
    {/* TODO: This feels clumsy / complicated. Is there a better way than using id (maybe not)?
       Should we consider passing just data to update, not the whole object, so we don't have to
       create new object here? Maybe we can change this update, or have a second update method. */}
    this.props.taskList.map(
      (t) => this.props.updateTask(t.id, new Task ({ ...t.toData(), isDone: !areAllDone }))
    )
  }

  deleteCompletedTasks = () => {
    this.props.taskList.map((t) => { if (t.isDone) this.props.removeTask(t.id) })
  }

  render = () => {
    return (
      <div className="mainContainer">
        <h1> { config.appName } </h1>

        <button onClick={this.toggleIsDoneForAllTasks}>
            Toggle completion {/* TODO: Use icon (but we need to either install @material-ui/icons 
            or add font-awesome to the index.html.  */}
        </button>

        <NewTaskForm
          onCreate={task => this.props.addTask(task)}
          submitButtonLabel={'Create new task'}
        />

        <div className="taskListContainer">
          <TaskList editable />
        </div>

        <div className="footer">
          { this.props.taskList.filter(task => !task.isDone).length } items left

          { this.props.taskList.some(t => t.isDone) &&
            <button onClick={this.deleteCompletedTasks}>Clear completed</button>
          }
        </div>
      </div>
    )
  }
}
