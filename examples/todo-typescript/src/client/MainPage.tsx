import './Main.css';
import React from 'react';
import logout from '@wasp/auth/logout.js';
import useAuth from '@wasp/auth/useAuth.js';
import { useQuery } from '@wasp/queries'; // Wasp uses a thin wrapper around react-query
import getTasks from '@wasp/queries/getTasks';
import createTask from '@wasp/actions/createTask';
import updateTask from '@wasp/actions/updateTask';
import waspLogo from './waspLogo.png';
import { Task } from './types'

const MainPage = () => {
  const { data: user } = useAuth();
  const { data: tasks, isLoading, error } = useQuery(getTasks);

  React.useEffect(() => {
    console.log(user);
  }, [user]);

  if (isLoading) return 'Loading...';
  if (error) return 'Error: ' + error;

  return (
    <main>
      <img src={waspLogo} alt='wasp logo' />
      <h1>
        {user.username}
        {`'s tasks :)`}
      </h1>
      <NewTaskForm />
      {tasks && <TasksList tasks={tasks} /> }
      <button onClick={logout}> Logout </button>
    </main>
  );
};

const Todo = ({task, number}: {task: Task, number: number}) => { 
  const handleIsDoneChange = async (event: React.ChangeEvent<HTMLInputElement>) => {
    try {
      await updateTask({
        taskId: task.id,
        isDone: event.currentTarget.checked,
      });
    } catch (err: any) {
      window.alert('Error while updating task: ' + err?.message);
    }
  };

  return (
    <div>
      <span>
        {number + 1}
        {''}
      </span>
      <input type='checkbox' id={task.id.toString()} checked={task.isDone} onChange={handleIsDoneChange} />
      <span>{task.description}</span>{' '}
    </div>
  );
};

const TasksList = ({tasks}: { tasks: Task[] }) => {
  if (tasks.length === 0) return <p>No tasks yet.</p>;
  return (
    <div className='tasklist'>
      {tasks.map((tsk, idx) => (
        <Todo task={tsk} number={idx} key={idx} />
      ))}
    </div>
  );
};

const NewTaskForm = () => {
  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    
    try {
      const description = event.currentTarget.description.value;
      console.log(description)
      event.currentTarget.reset();
      await createTask({ description });
    } catch (err: any) {
      window.alert('Error: ' + err?.message);
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <input name='description' type='text' defaultValue='' />
      <input type='submit' value='Create task' />
    </form>
  );
};

export default MainPage;
