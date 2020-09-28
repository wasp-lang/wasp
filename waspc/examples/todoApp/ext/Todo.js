import React, { useState } from 'react'

import { useQuery } from '@wasp/queries'
import getTasks from '@wasp/queries/getTasks.js'
import createTask from '@wasp/actions/createTask.js'
import updateTaskIsDone from '@wasp/actions/updateTaskIsDone.js'

const Todo = (props) => {
  const defaultNewTaskDescription = ''

  const [newTaskDescription, setNewTaskDescription] = useState(defaultNewTaskDescription)

  const { data: tasks, refetch, isFetching, isError, error: tasksError } = useQuery(getTasks)

  const isAnyTaskCompleted = () => tasks?.some(t => t.isDone)

  const isThereAnyTask = () => tasks?.length > 0

  const createNewTask = async (description) => {
    const task = { isDone: false, description }
    await createTask(task)
    refetch()
  }

  const handleNewTaskSubmit = async (event) => {
    event.preventDefault()
    try {
      await createNewTask(newTaskDescription)
      setNewTaskDescription(defaultNewTaskDescription)
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }
  }

  const Tasks = (props) => {
    return <div>
      { props.tasks.map((task, idx) => <Task task={task} key={idx}/>) }
    </div>
  }

  const TasksError = (props) => {
    return 'Error during fetching tasks: ' + (tasksError?.message || '')
  }

  const handleTaskIsDoneChange = async (event) => {
    const taskId = parseInt(event.target.id)
    const newIsDoneVal = event.target.checked

    try {
      await updateTaskIsDone({ taskId, newIsDoneVal })
      refetch()
    } catch (err) {
      console.log(err)
      window.alert('Error:' + err.message)
    }
  }

  const Task = (props) => {
    return <div>
      <input
        type="checkbox"
        id={props.task.id}
        checked={props.task.isDone}
        onChange={handleTaskIsDoneChange}
      />
      <span> { props.task.description } </span>
    </div>
  }

  return (
    <div className="todos">
      <div className="todos__container">
        <h1> Todos </h1>

        <div className="todos__toggleAndInput">
          <button
            disabled={!isThereAnyTask()}
            className="todos__toggleButton"
          > ✓ </button>

          <form onSubmit={handleNewTaskSubmit}>
            <input type="text"
                   value={newTaskDescription}
                   onChange={e => setNewTaskDescription(e.target.value)}
            />
            <input type="submit" value="Create new task"/>
          </form>
        </div>

        { isFetching && 'Fetching tasks...'}

        { isError && <TasksError/> }

        { isThereAnyTask() && (<>
          <Tasks tasks={tasks}/>

          <div className="todos__footer">
            <div className="todos__footer__itemsLeft">
              { tasks.filter(t => !t.isDone).length } items left
            </div>

            <div className="todos__footer__clearCompleted">
              <button className={isAnyTaskCompleted() ? '' : 'hidden' }> Delete completed </button>
            </div>
          </div>
        </>)}
      </div>
    </div>
  )
}

export default Todo
