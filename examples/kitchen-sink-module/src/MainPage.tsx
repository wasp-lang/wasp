import { useState } from "react";
import { moduleTodos } from "wasp/client/crud";
import { addRandomTodo, getTodoItems, useQuery } from "wasp/client/operations";
import "./MainPage.css";
import {
  getCurrentPathname,
  pingModuleApi,
  requestModuleJob,
} from "./moduleApiClient";
import {
  getModuleApiPrefix,
  getModuleJobApiPath,
  getModulePingApiPath,
  MODULE_API_HEADER_NAME,
} from "./moduleApiContract";
import type { TodoItem, TodoItems } from "./types";

export function MainPage() {
  const modulePrefix = getCurrentPathname();

  return (
    <main className="fsm-module-page">
      <p className="fsm-module-declaration-tag">Wasp full-stack module</p>
      <h1>TODO quotes module</h1>
      <p className="fsm-module-intro">
        This page ships from the <code>@kitchen-sink/module</code> package as
        its <code>route</code> + <code>page</code> declarations.
      </p>

      <OperationsTodoSection />
      <CrudTodoSection />
      <PingSection modulePrefix={modulePrefix} />
      <JobSection modulePrefix={modulePrefix} />
    </main>
  );
}

function OperationsTodoSection() {
  const { data, isLoading, error } = useQuery(getTodoItems);
  const [isAdding, setIsAdding] = useState(false);
  const [addError, setAddError] = useState<string | null>(null);

  const todoItems: TodoItems | undefined = data;

  async function handleAddRandomTodo() {
    setIsAdding(true);
    setAddError(null);

    try {
      await addRandomTodo();
    } catch (error) {
      setAddError(getErrorMessage(error));
    } finally {
      setIsAdding(false);
    }
  }

  return (
    <section className="fsm-module-section">
      <p className="fsm-module-declaration-tag">Query + action</p>
      <div className="fsm-module-section-header">
        <div>
          <h2>TODO list via operations</h2>
          <p className="fsm-module-section-copy">
            Query <code>getTodoItems</code> reads the latest five, action{" "}
            <code>addRandomTodo</code> adds a random quote.
          </p>
        </div>
        <button
          type="button"
          onClick={handleAddRandomTodo}
          disabled={isAdding}
          className="fsm-module-primary-button"
        >
          {isAdding ? "Adding..." : "Add one random TODO"}
        </button>
      </div>

      {isLoading && <p className="fsm-module-section-copy">Loading TODOs...</p>}
      {error && (
        <p className="fsm-module-error-text">
          Failed to load TODOs: {error.message}
        </p>
      )}
      {addError && (
        <p className="fsm-module-error-text">Failed to add TODO: {addError}</p>
      )}

      {todoItems &&
        (todoItems.items.length === 0 ? (
          <p className="fsm-module-section-copy">No TODOs yet.</p>
        ) : (
          <>
            <ul className="fsm-module-todo-list">
              {todoItems.items.map((item) => (
                <li
                  key={item.id}
                  className="fsm-module-todo-row"
                  data-done={item.isDone}
                >
                  <span className="fsm-module-todo-text">
                    {item.description}
                  </span>
                </li>
              ))}
            </ul>
            <p className="fsm-module-section-copy">
              {todoItems.items.length} of {todoItems.totalCount} TODOs
            </p>
          </>
        ))}
    </section>
  );
}

function CrudTodoSection() {
  const crudQuery = moduleTodos.getAll.useQuery();
  const [crudError, setCrudError] = useState<string | null>(null);

  const crudTodoItems: TodoItem[] | undefined = crudQuery.data;

  async function handleToggleDone(item: TodoItem) {
    setCrudError(null);

    try {
      await moduleTodos.update.action({ id: item.id, isDone: !item.isDone });
    } catch (error) {
      setCrudError(getErrorMessage(error));
    }
  }

  async function handleDelete(item: TodoItem) {
    setCrudError(null);

    try {
      await moduleTodos.delete.action({ id: item.id });
    } catch (error) {
      setCrudError(getErrorMessage(error));
    }
  }

  return (
    <section className="fsm-module-section">
      <p className="fsm-module-declaration-tag">Crud</p>
      <h2>TODO list via CRUD</h2>
      <p className="fsm-module-section-copy">
        Same Tasks via the <code>moduleTodos</code> CRUD: custom{" "}
        <code>getAll</code>, default <code>update</code> and <code>delete</code>
        .
      </p>

      {crudQuery.isLoading && (
        <p className="fsm-module-section-copy">Loading TODOs...</p>
      )}
      {crudQuery.error && (
        <p className="fsm-module-error-text">
          Failed to load TODOs: {crudQuery.error.message}
        </p>
      )}
      {crudError && (
        <p className="fsm-module-error-text">
          Failed to update TODO: {crudError}
        </p>
      )}

      {crudTodoItems && (
        <ul data-testid="module-crud-list" className="fsm-module-todo-list">
          {crudTodoItems.length === 0 && (
            <li className="fsm-module-todo-row fsm-module-todo-row-empty">
              No TODOs yet.
            </li>
          )}
          {crudTodoItems.map((item) => (
            <li
              key={item.id}
              data-testid="module-crud-item"
              data-done={item.isDone}
              className="fsm-module-todo-row"
            >
              <span className="fsm-module-todo-text">{item.description}</span>
              <button
                type="button"
                className="fsm-module-small-button"
                onClick={() => handleToggleDone(item)}
              >
                Toggle done
              </button>
              <button
                type="button"
                className="fsm-module-small-button"
                onClick={() => handleDelete(item)}
              >
                Delete
              </button>
            </li>
          ))}
        </ul>
      )}
    </section>
  );
}

function PingSection({ modulePrefix }: { modulePrefix: string }) {
  const [isPinging, setIsPinging] = useState(false);
  const [moduleApiHeader, setModuleApiHeader] = useState<string | null>(null);
  const [pingError, setPingError] = useState<string | null>(null);

  async function handlePingModuleApi() {
    setIsPinging(true);
    setModuleApiHeader(null);
    setPingError(null);

    try {
      const response = await pingModuleApi();
      setModuleApiHeader(response.moduleApiHeader);
    } catch (error) {
      setPingError(getErrorMessage(error));
    } finally {
      setIsPinging(false);
    }
  }

  return (
    <section className="fsm-module-section">
      <p className="fsm-module-declaration-tag">Api + apiNamespace</p>
      <h2>GET {getModulePingApiPath(modulePrefix)}</h2>
      <p className="fsm-module-section-copy">
        The apiNamespace middleware stamps <code>{MODULE_API_HEADER_NAME}</code>{" "}
        on every response under <code>{getModuleApiPrefix(modulePrefix)}</code>.
      </p>
      <button
        type="button"
        onClick={handlePingModuleApi}
        disabled={isPinging}
        className="fsm-module-primary-button"
      >
        {isPinging ? "Pinging..." : "Ping module API"}
      </button>
      {moduleApiHeader && (
        <p data-testid="module-api-header" className="fsm-module-result-text">
          <code>
            {MODULE_API_HEADER_NAME}: {moduleApiHeader}
          </code>
        </p>
      )}
      {pingError && (
        <p className="fsm-module-error-text">
          Failed to ping module API: {pingError}
        </p>
      )}
    </section>
  );
}

function JobSection({ modulePrefix }: { modulePrefix: string }) {
  const [isStartingJob, setIsStartingJob] = useState(false);
  const [jobId, setJobId] = useState<string | null>(null);
  const [jobError, setJobError] = useState<string | null>(null);

  async function handleStartJob() {
    setIsStartingJob(true);
    setJobId(null);
    setJobError(null);

    try {
      const response = await requestModuleJob("module-page");
      setJobId(response.jobId);
    } catch (error) {
      setJobError(getErrorMessage(error));
    } finally {
      setIsStartingJob(false);
    }
  }

  return (
    <section className="fsm-module-section">
      <p className="fsm-module-declaration-tag">Job</p>
      <h2>POST {getModuleJobApiPath(modulePrefix)}</h2>
      <p className="fsm-module-section-copy">
        Submits the module's PgBoss job and returns its ID.
      </p>
      <button
        type="button"
        onClick={handleStartJob}
        disabled={isStartingJob}
        className="fsm-module-primary-button"
      >
        {isStartingJob ? "Submitting..." : "Submit module job"}
      </button>
      {jobId && (
        <p data-testid="module-job-id" className="fsm-module-result-text">
          Submitted job: {jobId}
        </p>
      )}
      {jobError && (
        <p className="fsm-module-error-text">
          Failed to submit job: {jobError}
        </p>
      )}
    </section>
  );
}

function getErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}
