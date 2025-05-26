import { useParams } from "react-router-dom";
import { Link } from "wasp/client/router";

import { tasks as tasksCrud } from "wasp/client/crud";

export const DetailPage = () => {
  const { id } = useParams<{ id: string }>();
  const { data: task, isLoading } = tasksCrud.get.useQuery(
    {
      id: parseInt(id!, 10),
    },
    {
      enabled: !!id,
    },
  );

  return (
    <div className="container">
      <main>
        <h1>Tasks (CRUD feature) - Detail page</h1>
        <div className="tasks">
          {isLoading && <div>Loading...</div>}
          {task && (
            <div key={task.id} className="p-4 border my-4">
              <>
                <div className="task__title">
                  {JSON.stringify(task, null, 2)}
                </div>
              </>
            </div>
          )}
        </div>
        <Link to="/crud">Return</Link>
      </main>
    </div>
  );
};
