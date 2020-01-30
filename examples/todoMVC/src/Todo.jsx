import React from 'react'

import Task from '@wasp/entities/task/Task'
import NewTaskForm from '@wasp/entities/task/components/NewTaskForm'
import TaskList from '@wasp/entities/task/components/TaskList'

import * as config from './config'

const TASK_FILTER_TYPES = Object.freeze({
  ALL: 'all',
  ACTIVE: 'active',
  COMPLETED: 'completed'
})

const TASK_FILTERS = Object.freeze({
  [TASK_FILTER_TYPES.ALL]: null,
  [TASK_FILTER_TYPES.ACTIVE]: task => !task.isDone,
  [TASK_FILTER_TYPES.COMPLETED]: task => task.isDone
})

export default class Todo extends React.Component {

  state = {
    taskFilterName: TASK_FILTER_TYPES.ALL
  }

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

  TaskFilterButton = ({ filterType, label }) => (
    <button
      className={this.state.taskFilterName === filterType ? 'selected' : null}
      onClick={() => this.setState({ taskFilterName: filterType })}
    >
      {label}
    </button>
  )

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
          <TaskList
            editable
            filter={TASK_FILTERS[this.state.taskFilterName]}
          />
        </div>

        <div className="footer">
          <div>
            { this.props.taskList.filter(task => !task.isDone).length } items left

            { this.props.taskList.some(t => t.isDone) &&
                <button onClick={this.deleteCompletedTasks}>Clear completed</button>
            }
          </div>
          <div>
            <this.TaskFilterButton filterType={TASK_FILTER_TYPES.ALL} label="All" />
            <this.TaskFilterButton filterType={TASK_FILTER_TYPES.ACTIVE} label="Active" />
            <this.TaskFilterButton filterType={TASK_FILTER_TYPES.COMPLETED} label="Completed" />
          </div>
        </div>
      </div>
    )
  }
}
