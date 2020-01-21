import React from 'react'

import NewTaskForm from '@wasp/entities/task/components/NewTaskForm'
import TaskList from '@wasp/entities/task/components/List'

import * as config from './config'


export default class Todo extends React.Component {
  render = () => {
    return <div className="mainContainer">
      <h1> { config.appName } </h1>

      <NewTaskForm
        onCreate={task => this.props.addTask(task)}
        submitButtonLabel={'Create new task'}
      />

      <div className="taskListContainer">
        <TaskList editable />
      </div>

      <div className="footer">
        { this.props.taskList.filter(task => !task.isDone).length } items left
      </div>
    </div>
  }
}
