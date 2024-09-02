---
title: 'Wasp: The JavaScript Answer to Django for Web Development'
authors: [sam]
image: /img/django-vs-wasp/wasp-django-banner.png
tags: [webdev, auth, react, django, tutorial, full-stack]
---
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';
import ImgWithCaption from './components/ImgWithCaption'

![a django dev tries wasp](/img/django-vs-wasp/wasp-django-banner.png)

## Wasp vs Django: Building a full stack application just got a lot easier

Hey, I’m Sam, a backend engineer with a lot of experience with Django. I wanted to make the jump and learn some frontend for a full stack app. I quickly experienced the arduous nature of a React-with-Django project and thought the pain was just part of the development process. However, I came across a very cool new full stack framework called [Wasp](https://wasp-lang.dev/). 

Wasp is an amazing dev tool for full stack applications. Combining things like React, Node.js and Prisma, Wasp allows for development to be expedited in ways never before seen.

In this article, I am going to walk through creating a full stack application in Django versus Wasp to prove the simplicity of Wasp against a very conventional full-stack technology. I am also going to make a React frontend connected to Django. The point is to highlight the inefficiencies, difficulties, and issues that can (and will) arise with Django/React that are made vastly simpler when working with Wasp.

This article is not intended as a how-to, but I do provide code snippets to give you a feel for their differences. Also note that in order to give a side-by-side comparison, I'll use tabs which you can switch back and forth between, like this:

<Tabs>
  <TabItem value="Django" label="Django 🟢" default>
    Django info will go here...
  </TabItem>
  <TabItem value="Wasp" label="Wasp 🐝">
    ...and the Wasp comparison here.
  </TabItem>
</Tabs>


<ImgWithCaption
    alt="Let's get started"
    source="https://i.giphy.com/BpGWitbFZflfSUYuZ9.webp"
/>

## Part 1: Let There Be Light!

### Let’s create some projects and set things up

This part is about the only part where there is significant overlap between Django and Wasp. Both starting from the terminal, let’s make a simple task app (I am assuming you have Django and [Wasp installed](https://wasp-lang.dev/docs/quick-start) and in your path).


<Tabs>
<TabItem value="Django" label="Django 🟢" default>

```sh title="terminal"
django-admin startproject
python manage.py starapp Todo
```

</TabItem>
<TabItem value="Wasp" label="Wasp 🐝">

```sh title="terminal"
wasp new Todo
wasp 
```

</TabItem>
</Tabs>

Now Wasp starts hot out of the gate. After running `wasp new` you'll see a menu, as shown below. Wasp can either start a basic app for you, or you can select from a multitude of pre-made templates (including a [fully functioning SaaS app](https://opensaas.sh)) or even use an AI-generated app based on your description!

![wasp cli menu](/img/django-vs-wasp/wasp-cli-menu.png)

Meanwhile, Django works as a project with apps within the project (again, this is essentially all for backend operations) and there can be many apps to one Django project. Thus, you have to register each app in the Django project settings. 

```sh title="terminal"
python `manage.py` startapp todo
```

```py title="settings.py"
INSTALLED_APPS [
...
'Todo'
]
```

### Database Time

So now we need a database, and this is another area where Wasp really shines. With Django, we need to create a model in the `models.py` file.  Wasp, meanwhile, uses Prisma as it's ORM which allows us to clearly define necessary fields and make database creation simple in an easy to understand way.

<Tabs>
<TabItem value="Django" label="Django 🟢" default>

```py title="models.py"
from django.db import models

class Task(models.Model):
    title = models.CharField(max_length=200)
    completed = models.BooleanField(default=False)

    def __str__(self):
        return self.title
```

</TabItem>
<TabItem value="Wasp" label="Wasp 🐝">

```jsx title="schema.prisma"
model Task {
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
}
```

</TabItem>
</Tabs>

Django and Wasp do share similar ways to migrate databases:

<Tabs>
<TabItem value="Django" label="Django 🟢" default>

```sh
python manage.py makemigrations
python manage.py migrate
```

</TabItem>
<TabItem value="Wasp" label="Wasp 🐝">

```sh
wasp db migrate-dev
```

</TabItem>
</Tabs>

But with Wasp, you can also do some pretty nifty database stuff that Django can't.

Right now we're using SQLite, but how about instantly setting up a development Posgres database? Wasp can do that with:

```sh 
wasp db start
```

That's it! With that you've got a docker container running a Postgres instance and it's instantly connected to your Wasp app. 🤯

Or what if you want to see your database in real time via Prisma’s database studio UI, something not possible without third party extensions with Django. For that, just run:

```sh
wasp db studio
```

Are you starting to see what I mean now?

### Routes

Routes in Django and Wasp follow a shomewhat similar pattern. However, if you're familiar with React, then Wasp’s system is far superior. 

- Django works through the backend (`views.py`, which I will get to later in this article) which do all the CRUD operations. Those view functions are associated to a specific route within an app within a project (I know, a lot), and it can get more complicated if you start using primary keys and IDs. You need to create a `urls.py` file and direct your specific views file and functions to a route. Those app urls are then connected to the project urls. Phew.
- Wasp’s way: define a route and direct it to a component.

<Tabs>
<TabItem value="Django" label="Django 🟢" default>

```py title="todo/urls.py"
from django.urls import path
from . import views

urlpatterns = [
    path('', views.index, name='index'),
    path('update/<str:pk>/', views.updateTask, name='update_task'),
    path('delete/<str:pk>/', views.deleteTask, name='delete_task'),
```

```py title="./urls.py"
from django.contrib import admin
from django.urls import path, include

urlpatterns = [
    path('admin/', admin.site.urls),
    path('', include('todo.urls')),
]
```

</TabItem>
<TabItem value="Wasp" label="Wasp 🐝">

```jsx title="main.wasp"
route TaskRoute { path: "/", to: TaskPage }
page TaskPage {
  component: import { TaskPage } from "@src/TaskPage"
}
```

</TabItem>
</Tabs>


### CRUD

Ok, this is where the benefits of Wasp are about to become even more apparent. 

Firstly, I am going to revisit the `views.py` file. This is where magic is going to happen for Django backend. Here is a simple version of what the create, update, and delete functions could look like for our Task/Todo example:

<Tabs>
<TabItem value="Django" label="Django 🟢" default>

```py title="todo/views.py"
from django.shortcuts import render, redirect
from .models import Task
from .forms import TaskForm

def index(request):
    tasks = Task.objects.all()
    form = TaskForm()
    if request.method == 'POST':
        form = TaskForm(request.POST)
        if form.is_valid():
            form.save()
        return redirect('/')
    context = {'tasks': tasks, 'form': form}
    return render(request, 'todo/index.html', context)

def updateTask(request, pk):
    task = Task.objects.get(id=pk)
    form = TaskForm(instance=task)
    if request.method == 'POST':
        form = TaskForm(request.POST, instance=task)
        if form.is_valid():
            form.save()
        return redirect('/')
    context = {'form': form}
    return render(request, 'todo/update_task.html', context)

def deleteTask(request, pk):
    task = Task.objects.get(id=pk)
    if request.method == 'POST':
        task.delete()
        return redirect('/')
    context = {'task': task}
    return render(request, 'todo/delete.html', context)
```

```py title="app/forms.py"
from django import forms
from .models import Task

class TaskForm(forms.ModelForm):
    class Meta:
        model = Task
        fields = ['title', 'completed']
```

</TabItem>
<TabItem value="Wasp" label="Wasp 🐝">

```jsx title="main.wasp"
query getTasks {
  fn: import { getTasks } from "@src/operations",
  // Tell Wasp that this operation affects the `Task` entity. Wasp will automatically
  // refresh the client (cache) with the results of the operation when tasks are modified.
  entities: [Task]
}

action updateTask {
  fn: import { updateTask } from "@src/operations",
  entities: [Task]
}

action deleteTask {
  fn: import { deleteTask } from "@src/operations",
  entities: [Task]
}
```

```jsx title="operations.js"
export const getTasks = async (args, context) => {
  return context.entities.Task.findMany({
    orderBy: { id: 'asc' },
  })
}

export const updateTask = async ({ id, data }, context) => {
  return context.entities.Task.update({
    where: { id },
    data
  })
}

export const deleteTask = async ({ id }, context) => {
  return context.entities.Task.delete({
    where: { id }
  })
}
```

</TabItem>
</Tabs>

So right now, Wasp has a fully functioning backend with middleware configured for you. At this point we can create some React components, and then import and call these operations from the client. That is not the case with Django, unfortunately there is still a lot we need to do to configure React in our app and get things working together, which we will look at below.

## Part 2: So you want to use React with Django?

<ImgWithCaption
    alt="React with Django is... hard"
    source="https://i.giphy.com/LOVMoi1qYWJyw.webp"
/>

At this point we could just create a simple client with HTML and CSS to go with our Django app, but then this wouldn't be a fair comparison, as Wasp is a true full-stack framework and gives you a managed React-NodeJS-Prisma app out-of-the-box. So let's see what we'd have to do to get the same thing set up with Django.

Note that this section is going to highlight Django, so keep in mind that you can skip all the following steps if you just use Wasp. :)

**Django** 🟢

First thing’s first. Django needs a REST framework and CORS (Cross Origin Resource Sharing):

```sh title="terminal"
pip install djangorestframework
pip install django-cors-headers
```

Include Rest Framework and Cors Header as installed apps, CORS headers as middleware, and then also set a local host for the React frontend to be able to communicate with the backend (Django) server (again, there is no need to do any of this initial setup in Wasp as it's all handled for you):

```py title="settings.py"
INSTALLED_APPS = [
    ...
    'corsheaders',
]

MIDDLEWARE = [
    ...
    'corsheaders.middleware.CorsMiddleware',
    ...
]

CORS_ALLOWED_ORIGINS = [
    'http://localhost:3000',
]
```

And now a very important step, which is to serialize all the data from Django to be able to work in json format for React frontend

```py title="app/serializers.py"
from rest_framework import serializers
from .models import Task

class TaskSerializer(serializers.ModelSerializer):
    class Meta:
        model = Task
        fields = '__all__'
```

Now, since we are handling CRUD on the React side, we can change the views.py file: 

```py title="app/views.py"
from rest_framework import viewsets
from .models import Task
from .serializers import TaskSerializer

class TaskViewSet(viewsets.ModelViewSet):
    queryset = Task.objects.all()
    serializer_class = TaskSerializer
```
And now we need to change both app and project URLS since we have a frontend application on a different url than our backend. 

```py title="urls.py"
from django.contrib import admin
from django.urls import path, include

urlpatterns = [
    path('admin/', admin.site.urls),
    path('api/', include('todo.urls')),  # Add this line
]
```

```py title="todo/urls.py"
from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import TaskViewSet

router = DefaultRouter()
router.register(r'tasks', TaskViewSet)

urlpatterns = [
    path('', include(router.urls)),
]
```

By now you should be understanding why I've made the switch to using Wasp when building full-stack apps. Anyways, now we are actually able to make a React component with a Django backend 🙃

### React time

Ok, so now we can actually get back to comparing Wasp and Django.

<Tabs>
<TabItem value="Django" label="Django 🟢" default>

To start, lets create our React app in our Django project:

```sh title="terminal"
npx create-react-app frontend
```

Finally, we can make a component in React. A few things:

- I am using axios in the Django project here. Wasp comes bundled with [React-Query (aka Tanstack Query)](https://tanstack.com/query/v3), so the execution of (CRUD) operations is a lot more elegant and powerful.
- The api call is to my local server, obviously this will change in development.
- You can make this many different ways, I tried to keep it simple.

```jsx title="main.jsx"
import React, { useEffect, useState } from 'react';
import axios from 'axios';

const TaskList = () => {
  const [tasks, setTasks] = useState([]);
  const [newTask, setNewTask] = useState('');
  const [editingTask, setEditingTask] = useState(null);

  useEffect(() => {
      fetchTasks();
  }, []);

  const fetchTasks = () => {
    axios.get('http://127.0.0.1:8000/api/tasks/')
      .then(response => {
        setTasks(response.data);
      })
      .catch(error => {
        console.error('There was an error fetching the tasks!', error);
      });
  };

  const handleAddTask = () => {
    if (newTask.trim()) {
      axios.post('http://127.0.0.1:8000/api/tasks/', { title: newTask, completed: false })
        .then(() => {
          setNewTask('');
          fetchTasks();
        })
        .catch(error => {
          console.error('There was an error adding the task!', error);
        });
    }
  };

  const handleUpdateTask = (task) => {
    axios.put(`http://127.0.0.1:8000/api/tasks/${task.id}/`, task)
      .then(() => {
        fetchTasks();
        setEditingTask(null);
      })
      .catch(error => {
        console.error('There was an error updating the task!', error);
      });
  };

  const handleDeleteTask = (taskId) => {
    axios.delete(`http://127.0.0.1:8000/api/tasks/${taskId}/`)
      .then(() => {
        fetchTasks();
      })
      .catch(error => {
        console.error('There was an error deleting the task!', error);
      });
  };

  const handleEditTask = (task) => {
    setEditingTask(task);
  };

  const handleChange = (e) => {
    setNewTask(e.target.value);
  };

  const handleEditChange = (e) => {
    setEditingTask({ ...editingTask, title: e.target.value });
  };

  const handleEditCompleteToggle = () => {
    setEditingTask({ ...editingTask, completed: !editingTask.completed });
  };

  return (
    <div>
      <h1>To-Do List</h1>
      <input type="text" value={newTask} onChange={handleChange} placeholder="Add new task" />
      <button onClick={handleAddTask}>Add Task</button>
      <ul>
        {tasks.map(task => (
          <li key={task.id}>
            {editingTask && editingTask.id === task.id ? (
              <div>
                <input type="text" value={editingTask.title} onChange={handleEditChange} />
                <button onClick={() => handleUpdateTask(editingTask)}>Save</button>
                <button onClick={() => setEditingTask(null)}>Cancel</button>
                <button onClick={handleEditCompleteToggle}>
                  {editingTask.completed ? 'Mark Incomplete' : 'Mark Complete'}
                </button>
              </div>
          ) : (
            <div>
              {task.title} - {task.completed ? 'Completed' : 'Incomplete'}
              <button onClick={() => handleEditTask(task)}>Edit</button>
              <button onClick={() => handleDeleteTask(task.id)}>Delete</button>
            </div>
            )}
          </li>
        ))}
      </ul>
    </div>
  );
};

export default TaskList;
```

</TabItem>
<TabItem value="Wasp" label="Wasp 🐝">

And here's the Wasp React client for comparison. Take note how we're able to import the operations we defined earlier and call them here easily on the client with less configuration than the Django app. We also get the built-in caching power of the `useQuery` hook, as well as the ability to pass in our authenticated user as a prop (we'll get into this more below):

```jsx title="Main.jsx"
import React, { FormEventHandler, FormEvent } from "react";
import { type Task } from "wasp/entities";
import { type AuthUser, getUsername } from "wasp/auth";
import { logout } from "wasp/client/auth";
import { createTask, updateTask, deleteTasks, useQuery, getTasks } from "wasp/client/operations";
import waspLogo from "./waspLogo.png";

import "./Main.css";

export const MainPage = ({ user }) => {
  const { data: tasks, isLoading, error } = useQuery(getTasks);

  if (isLoading) return "Loading...";
  if (error) return "Error: " + error;

  const completed = tasks?.filter((task) => task.isDone).map((task) => task.id);

  return (
    <main>
      <img src={waspLogo} alt="wasp logo" />
      {user && user.identities.username && (
        <h1>
          {user.identities.username.id}
          {`'s tasks :)`}
        </h1>
      )}
      <NewTaskForm />
      {tasks && <TasksList tasks={tasks} />}
      <div className="buttons">
        <button
          className="logout"
          onClick={() => deleteTasks(completed ?? [])}
        >
          Delete completed
        </button>
        <button className="logout" onClick={logout}>
          Logout
        </button>
      </div>
    </main>
  );
};

function Todo({ id, isDone, description }) {
  const handleIsDoneChange = async (
    event
  ) => {
    try {
      await updateTask({
        id,
        isDone: event.currentTarget.checked,
      });
    } catch (err: any) {
      window.alert("Error while updating task " + err?.message);
    }
  };

  return (
    <li>
      <span className="todo-item">
        <input
          type="checkbox"
          id={id.toString()}
          checked={isDone}
          onChange={handleIsDoneChange}
        />
        <span>{description}</span>
        <button onClick={() => void deleteTasks([id])}>Delete</button>
      </span>
    </li>
  );
}

function TasksList({ tasks }) {
  if (tasks.length === 0) return <p>No tasks yet.</p>;
  return (
    <ol className="tasklist">
      {tasks.map((task, idx) => (
        <Todo {...task} key={idx} />
      ))}
    </ol>
  );
}

function NewTaskForm() {
  const handleSubmit = async (event) => {
    event.preventDefault();

    try {
      const description = event.currentTarget.description.value;
      console.log(description);
      event.currentTarget.reset();
      await createTask({ description });
    } catch (err: any) {
      window.alert("Error: " + err?.message);
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <input name="description" type="text" defaultValue="" />
      <input type="submit" value="Create task" />
    </form>
  );
}
```

</TabItem>
</Tabs>

<ImgWithCaption
    alt="Todo app"
    source="/img/django-vs-wasp/wasp-todo-app.gif"
/>

In the Wasp app you can see how much easier it is to call the server-side code via Wasp operations. Plus, Wasp gives you the added benefit of refreshing the client-side cache for the Entity that's referenced in the operation definition (in this case `Task`). And the cherry on top is how easy it is to pass the authenticated user to the component, something we haven't even touched on in the Django app, and which we will talk about more below.

## Part 3: Auth with Django? No way, José

<ImgWithCaption
    alt="Angry desk flip"
    source="https://i.giphy.com/4qx6IRdg26uZ3MTtRn.webp"
/>

So we already started to get a feel in the above code for how simple it is to pass an authenticated user around in Wasp. But how do we actually go about implementing full-stack Authentication in Wasp and Django.

This is one of Wasp’s biggest advantages. It couldn't be easier or more intuitive. On the other hand, the Django implementation is so long and complicated I'm not going to even bother showing you the code and I'll just list out the stps instead. Let's also look at Wasp first this time.

<Tabs>
<TabItem value="Wasp" label="Wasp 🐝" default>

```jsx title="main.wasp"
app TodoApp {
  wasp: {
    version: "^0.14.0"
  },

  title: "Todo App",

  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {}
    }
  }

  //...
```

That's it!

<ImgWithCaption
    alt="mind blown"
    source="/img/django-vs-wasp/mindblown.gif"
/>

</TabItem>
<TabItem value="Django" label="Django 🟢">

Let's check out what it takes to add a simple username and password auth implementation to a Django app (remember, this isn't even the code, just a checklist!):

1. **Install Necessary Packages:**
    - Use `pip` to install `djangorestframework`, `djoser`, and `djangorestframework-simplejwt` for Django.
    - Use `npm` to install `axios` and `jwt-decode` for React.
2. **Update Django Settings:**
    - Add required apps (`rest_framework`, `djoser`, `corsheaders`, and your Django app) to `INSTALLED_APPS`.
    - Configure middleware to include `CorsMiddleware`.
    - Set up Django REST Framework settings for authentication and permissions.
    - Configure SimpleJWT settings for token expiration and authorization header types.
3. **Set Up URL Routing:**
    - Include the `djoser` URLs for authentication and JWT endpoints in Django’s `urls.py`.
4. **Implement Authentication Context in React:**
    - Create an authentication context in React to manage login state and tokens.
    - Provide methods for logging in and out, and store tokens in local storage.
5. **Create Login Component in React:**
    - Build a login form component in React that allows users to enter their credentials and authenticate using the Django backend.
6. **Protect React Routes and Components:**
    - Use React Router to protect routes that require authentication.
    - Ensure that API requests include the JWT token for authenticated endpoints.
7. **Implement Task Update and Delete Functionality:**
    - Add methods in the React components to handle updating and deleting tasks.
    - Use `axios` to make PUT and DELETE requests to the Django API.
8. **Add Authentication to Django Views:**
    - Ensure that Django views and endpoints require authentication.
    - Use Django REST Framework permissions to protect API endpoints.

<ImgWithCaption
    alt="spongebob phew"
    source="https://c.tenor.com/dJCFZSkf3bAAAAAC/tenor.gif"
/>

</TabItem>
</Tabs>

And that's all it takes to implement full-stack [Auth with Wasp](https://wasp-lang.dev/docs/auth/overview)! But that's just one example, you can also add other auth methods easily, like `google: {}`, `gitHub: {}` and `discord: {}` social auth, after configuring the apps and adding your environment variables. 

Wasp allows you to get building without worrying about so many things. I don’t need to worry about password hashing, multiple projects and apps, CORS headers, etc. I just need to add a couple lines of code. 

**Wasp just makes sense.**

## One Final Thing

I just want to highlight one more aspect of Wasp that I really love. In Django, we're completely responsible for dealing with all the boilerplate code when setting up a new app. In other words, we have to set up new apps from scratch every time (even if it's been done before a million times by us and other devs). But with Wasp we can scaffold a new app template in a number of ways to *really* jump start the development process. 

Let's check out these other ways to get a full-stack app started in Wasp.

### Way #1: Straight Outta Terminal

A simple **wasp new**  in the terminal shows numerous starting options and app templates. If I really want to make a todo app for example, well there you have it, option 2. 

Right out of the box you have a to-do application with authentication, CRUD functionality, and some basic styling. All of this is ready to be amended for your specific use case. 

Or what if you want to turn code into money? Well, you can also get a [fully functioning SaaS app](https://opensaas.sh). Interested in the latest AI offereings? You also have a vector embeddings template, or an AI full-stack app protoyper! 5 options that save you from having to write a ton of boilerplate code.

![wasp cli menu](/img/django-vs-wasp/wasp-cli-menu.png)

### Way #2: [Mage.ai](https://usemage.ai/) (Free!)

Just throw a name, prompt, and select a few of your desired settings and boom, you get a fully functioning prototype app. From here you can use other other AI tools, like [Cursor's AI code editor](https://www.cursor.com/), to generate new features and help you debug! 

![mage](/img/django-vs-wasp/usemage.png)

:::note
💡 The Mage functionality is also achievable via the terminal (`wasp new -> ai-generated`), but you need to provide your own OpenAI api key for it to work.
:::

## Can you show us your support?

![https://media2.giphy.com/media/l0MYAs5E2oIDCq9So/giphy.gif?cid=7941fdc6l6i66eq1dc7i5rz05nkl4mgjltyv206syb0o304g&ep=v1_gifs_search&rid=giphy.gif&ct=g](https://media2.giphy.com/media/l0MYAs5E2oIDCq9So/giphy.gif?cid=7941fdc6l6i66eq1dc7i5rz05nkl4mgjltyv206syb0o304g&ep=v1_gifs_search&rid=giphy.gif&ct=g)

Are you interested in more content like this? Sign up for [our newsletter](https://wasp-lang.dev/#signup) and give us [a star on GitHub](https://www.github.com/wasp-lang/wasp)! We need your support to keep pushing our projects forward 😀

### Conclusion

So there you have it. As I said in the beginning, coming from Django I was amazed how easy it was to build full-stack apps with Wasp, which is what inspired me to write this article.

Hopefully I was able to show you why breaking away from Django in favor of Wasp can be beneficial in time, energy, and emotion.
