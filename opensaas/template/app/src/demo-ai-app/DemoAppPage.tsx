import { type Task } from "wasp/entities";

import {
  createTask,
  deleteTask,
  generateGptResponse,
  getAllTasksByUser,
  updateTask,
  useQuery,
} from "wasp/client/operations";
import { Link, routes } from "wasp/client/router";

import { ArrowRight, Loader2, Trash2 } from "lucide-react";
import { useMemo, useState } from "react";
import { Button } from "../client/components/ui/button";
import {
  Card,
  CardContent,
  CardHeader,
  CardTitle,
} from "../client/components/ui/card";
import { Checkbox } from "../client/components/ui/checkbox";
import { Input } from "../client/components/ui/input";
import { Label } from "../client/components/ui/label";
import { ToastAction } from "../client/components/ui/toast";
import { toast } from "../client/hooks/use-toast";
import { cn } from "../client/utils";
import type {
  GeneratedSchedule,
  Task as ScheduleTask,
  TaskItem,
  TaskPriority,
} from "./schedule";

export function DemoAppPage() {
  return (
    <div className="py-10 lg:mt-10">
      <div className="mx-auto max-w-7xl px-6 lg:px-8">
        <div className="mx-auto max-w-4xl text-center">
          <h2 className="text-foreground mt-2 text-4xl font-bold tracking-tight sm:text-5xl">
            <span className="text-primary">AI</span> Day Scheduler
          </h2>
        </div>
        <p className="text-muted-foreground mx-auto mt-6 max-w-2xl text-center text-lg leading-8">
          This example app uses OpenAI's chat completions with function calling
          to return a structured JSON object. Try it out, enter your day's
          tasks, and let AI do the rest!
        </p>
        {/* begin AI-powered Todo List */}
        <Card className="bg-muted/10 my-8">
          <CardContent className="mx-auto my-8 space-y-10 px-6 py-10 sm:w-[90%] md:w-[70%] lg:w-[50%]">
            <NewTaskForm handleCreateTask={createTask} />
          </CardContent>
        </Card>
        {/* end AI-powered Todo List */}
      </div>
    </div>
  );
}

function NewTaskForm({
  handleCreateTask,
}: {
  handleCreateTask: typeof createTask;
}) {
  const [description, setDescription] = useState<string>("");
  const [todaysHours, setTodaysHours] = useState<number>(8);
  const [response, setResponse] = useState<GeneratedSchedule | null>({
    tasks: [
      {
        name: "Respond to emails",
        priority: "high" as TaskPriority,
      },
      {
        name: "Learn WASP",
        priority: "low" as TaskPriority,
      },
      {
        name: "Read a book",
        priority: "medium" as TaskPriority,
      },
    ],
    taskItems: [
      {
        description: "Read introduction and chapter 1",
        time: 0.5,
        taskName: "Read a book",
      },
      {
        description: "Read chapter 2 and take notes",
        time: 0.3,
        taskName: "Read a book",
      },
      {
        description: "Read chapter 3 and summarize key points",
        time: 0.2,
        taskName: "Read a book",
      },
      {
        description: "Check and respond to important emails",
        time: 1,
        taskName: "Respond to emails",
      },
      {
        description: "Organize and prioritize remaining emails",
        time: 0.5,
        taskName: "Respond to emails",
      },
      {
        description: "Draft responses to urgent emails",
        time: 0.5,
        taskName: "Respond to emails",
      },
      {
        description: "Watch tutorial video on WASP",
        time: 0.5,
        taskName: "Learn WASP",
      },
      {
        description: "Complete online quiz on the basics of WASP",
        time: 1.5,
        taskName: "Learn WASP",
      },
      {
        description: "Review quiz answers and clarify doubts",
        time: 1,
        taskName: "Learn WASP",
      },
    ],
  });
  const [isPlanGenerating, setIsPlanGenerating] = useState<boolean>(false);

  const { data: tasks, isLoading: isTasksLoading } =
    useQuery(getAllTasksByUser);

  const handleSubmit = async () => {
    try {
      await handleCreateTask({ description });
      setDescription("");
    } catch (err) {
      const message =
        err instanceof Error ? err.message : "Something went wrong";
      window.alert("Error: " + message);
    }
  };

  const handleGeneratePlan = async () => {
    try {
      setIsPlanGenerating(true);
      const response = await generateGptResponse({
        hours: todaysHours,
      });
      if (response) {
        setResponse(response);
      }
    } catch (err) {
      const statusCode =
        err && typeof err === "object" && "statusCode" in err
          ? err.statusCode
          : undefined;
      if (statusCode === 402) {
        toast({
          title: "⚠️ You are out of credits!",
          style: {
            minWidth: "400px",
          },
          action: (
            <ToastAction
              altText="Go to pricing page to buy credits/subscription"
              asChild
            >
              <Link to={routes.PricingPageRoute.to}>
                Go to pricing page <ArrowRight className="ml-1 h-4 w-4" />
              </Link>
            </ToastAction>
          ),
        });
      } else {
        toast({
          title: "Error",
          description:
            err instanceof Error ? err.message : "Something went wrong",
          variant: "destructive",
        });
      }
    } finally {
      setIsPlanGenerating(false);
    }
  };

  return (
    <div className="flex flex-col justify-center gap-10">
      <div className="flex flex-col gap-3">
        <div className="flex items-center justify-between gap-3">
          <Input
            type="text"
            id="description"
            className="flex-1"
            placeholder="Enter task description"
            value={description}
            onChange={(e) => setDescription(e.currentTarget.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter") {
                handleSubmit();
              }
            }}
          />
          <Button
            type="button"
            onClick={handleSubmit}
            disabled={!description}
            variant="default"
            size="default"
          >
            Add Task
          </Button>
        </div>
      </div>

      <div className="col-span-full space-y-10">
        {isTasksLoading && (
          <div className="text-muted-foreground">Loading...</div>
        )}
        {tasks! && tasks.length > 0 ? (
          <div className="space-y-4">
            {tasks.map((task: Task) => (
              <Todo
                key={task.id}
                id={task.id}
                isDone={task.isDone}
                description={task.description}
                time={task.time}
              />
            ))}
            <div className="flex flex-col gap-3">
              <div className="flex items-center justify-between gap-3">
                <Label
                  htmlFor="time"
                  className="text-muted-foreground text-nowrap text-sm font-semibold"
                >
                  How many hours will you work today?
                </Label>
                <Input
                  type="number"
                  id="time"
                  step={0.5}
                  min={1}
                  max={24}
                  className="min-w-28 text-center"
                  value={todaysHours}
                  onChange={(e) => setTodaysHours(+e.currentTarget.value)}
                />
              </div>
            </div>
          </div>
        ) : (
          <div className="text-muted-foreground text-center">
            Add tasks to begin
          </div>
        )}
      </div>

      <Button
        type="button"
        disabled={isPlanGenerating || tasks?.length === 0}
        onClick={() => handleGeneratePlan()}
        variant="default"
        size="default"
        className="w-full"
        data-testid="generate-schedule-button"
      >
        {isPlanGenerating ? (
          <>
            <Loader2 className="mr-2 inline-block animate-spin" />
            Generating...
          </>
        ) : (
          "Generate Schedule"
        )}
      </Button>

      {!!response && (
        <div className="flex flex-col">
          <h3 className="text-foreground mb-4 text-lg font-semibold">
            Today's Schedule
          </h3>
          <Schedule schedule={response} />
        </div>
      )}
    </div>
  );
}

type TodoProps = Pick<Task, "id" | "isDone" | "description" | "time">;

function Todo({ id, isDone, description, time }: TodoProps) {
  const handleCheckboxChange = async (checked: boolean) => {
    await updateTask({
      id,
      isDone: checked,
    });
  };

  const handleTimeChange = async (e: React.ChangeEvent<HTMLInputElement>) => {
    await updateTask({
      id,
      time: e.currentTarget.value,
    });
  };

  const handleDeleteClick = async () => {
    await deleteTask({ id });
  };

  return (
    <Card className="p-4">
      <div className="flex w-full items-center justify-between">
        <div className="flex w-full items-center justify-between gap-5">
          <div className="flex items-center gap-3">
            <Checkbox
              checked={isDone}
              onCheckedChange={handleCheckboxChange}
              className="data-[state=checked]:bg-primary data-[state=checked]:border-primary"
            />
            <span
              className={cn("text-foreground", {
                "text-muted-foreground line-through": isDone,
              })}
            >
              {description}
            </span>
          </div>
          <div className="flex items-center gap-2">
            <Input
              id="time"
              type="number"
              min={0.5}
              step={0.5}
              className={cn("w-18 h-8 text-center text-xs", {
                "pointer-events-none opacity-50": isDone,
              })}
              value={time}
              onChange={handleTimeChange}
            />
            <span
              className={cn("text-muted-foreground text-xs italic", {
                "text-muted-foreground": isDone,
              })}
            >
              hrs
            </span>
          </div>
        </div>
        <div className="w-15 flex items-center justify-end">
          <Button
            variant="ghost"
            size="sm"
            onClick={handleDeleteClick}
            title="Remove task"
            className="text-destructive hover:text-destructive/80 h-auto p-1"
          >
            <Trash2 size="20" />
          </Button>
        </div>
      </div>
    </Card>
  );
}

function Schedule({ schedule }: { schedule: GeneratedSchedule }) {
  return (
    <div className="flex flex-col gap-6 py-6" data-testid="schedule">
      <div className="space-y-4">
        {schedule.tasks ? (
          schedule.tasks
            .map((task) => (
              <TaskCard
                key={task.name}
                task={task}
                taskItems={schedule.taskItems}
              />
            ))
            .sort((a, b) => {
              const priorityOrder: TaskPriority[] = ["low", "medium", "high"];
              if (a.props.task.priority && b.props.task.priority) {
                return (
                  priorityOrder.indexOf(b.props.task.priority) -
                  priorityOrder.indexOf(a.props.task.priority)
                );
              } else {
                return 0;
              }
            })
        ) : (
          <div className="text-muted-foreground text-center">
            OpenAI didn't return any Tasks. Try again.
          </div>
        )}
      </div>
    </div>
  );
}

function TaskCard({
  task,
  taskItems,
}: {
  task: ScheduleTask;
  taskItems: TaskItem[];
}) {
  const taskPriorityToColorMap: Record<TaskPriority, string> = {
    high: "bg-destructive/10 border-destructive/20 text-red-500",
    medium: "bg-warning/10 border-warning/20 text-warning",
    low: "bg-success/10 border-success/20 text-success",
  };

  return (
    <Card className={cn("border-2", taskPriorityToColorMap[task.priority])}>
      <CardHeader className="pb-3">
        <CardTitle className="flex items-center justify-between text-base">
          <span>{task.name}</span>
          <span className="text-xs font-medium italic">
            {" "}
            {task.priority} priority
          </span>
        </CardTitle>
      </CardHeader>
      <CardContent className="pt-0">
        {taskItems ? (
          <ul className="space-y-2">
            {taskItems.map((taskItem) => {
              if (taskItem.taskName === task.name) {
                return (
                  <TaskCardItem key={taskItem.description} {...taskItem} />
                );
              }
              return null;
            })}
          </ul>
        ) : (
          <div className="text-muted-foreground text-center">
            OpenAI didn't return any Task Items. Try again.
          </div>
        )}
      </CardContent>
    </Card>
  );
}

function TaskCardItem({ description, time }: TaskItem) {
  const [isDone, setIsDone] = useState<boolean>(false);

  const formattedTime = useMemo(() => {
    if (time === 0) return "0min";
    const hours = Math.floor(time);
    const minutes = Math.round((time - hours) * 60);

    const parts: string[] = [];
    if (hours > 0) parts.push(`${hours}hr`);
    if (minutes > 0) parts.push(`${minutes}min`);

    return parts.join(" ");
  }, [time]);

  const handleCheckedChange = (checked: boolean | "indeterminate") => {
    setIsDone(checked === true);
  };

  return (
    <li className="flex items-center justify-between gap-4 rounded-md p-2">
      <div className="flex flex-1 items-center gap-3">
        <Checkbox
          checked={isDone}
          onCheckedChange={handleCheckedChange}
          className="data-[state=checked]:bg-primary data-[state=checked]:border-primary"
        />
        <span
          className={cn("text-sm leading-tight", {
            "text-muted-foreground line-through opacity-50": isDone,
          })}
        >
          {description}
        </span>
      </div>
      <span
        className={cn("text-muted-foreground text-sm", {
          "line-through opacity-50": isDone,
        })}
      >
        {formattedTime}
      </span>
    </li>
  );
}
