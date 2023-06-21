import './Main.css';
import React, { useEffect, FormEventHandler, FormEvent } from 'react';
import logout from '@wasp/auth/logout';
import useAuth from '@wasp/auth/useAuth';
import { useQuery } from '@wasp/queries'; // Wasp uses a thin wrapper around react-query
import getTasks from '@wasp/queries/getTasks';
import createTask from '@wasp/actions/createTask';
import updateTask from '@wasp/actions/updateTask';
import waspLogo from './waspLogo.png';
import { Task } from './types'

export function MainPage() {
  const { data: user } = useAuth();
  const { data: tasks, isLoading, error } = useQuery<unknown, Task[]>(getTasks);

  useEffect(() => {
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

function Todo({ id, isDone, description }: Task) {
  const handleIsDoneChange: FormEventHandler<HTMLInputElement> = async (event) => {
    try {
      await updateTask({
        id,
        isDone: event.currentTarget.checked,
      });
    } catch (err: any) {
      window.alert('Error while updating task ' + err?.message);
    }
  }

  return (
    <li>
      <input type='checkbox' id={id.toString()} checked={isDone} onChange={handleIsDoneChange} />
      <span>{description}</span>
    </li>
  );
};

function TasksList({tasks}: { tasks: Task[] }) {
  if (tasks.length === 0) return <p>No tasks yet.</p>;
  return (
    <ol className='tasklist'>
      {tasks.map((task, idx) => (
        <Todo {...task} key={idx} />
      ))}
    </ol>
  );
};

function NewTaskForm() {
  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
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
