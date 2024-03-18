import "./Main.css";
import React from "react";
import { useParams } from "react-router-dom";
import { Link } from "wasp/client/router";

import { tasks as tasksCrud } from "wasp/client/crud";

const DetailPage = () => {
  const { id } = useParams<{ id: string }>();
  const { data: task, isLoading } = tasksCrud.get.useQuery({
    id: parseInt(id, 10),
  });

  return (
    <div className="container">
      <main>
        <h1>Tasks master</h1>
        <div className="tasks">
          {isLoading && <div>Loading...</div>}
          {task && (
            <div key={task.id} className="task">
              <>
                <div className="task__title">
                  {JSON.stringify(task, null, 2)}
                </div>
              </>
            </div>
          )}
        </div>
        <Link to="/">Return</Link>
      </main>
    </div>
  );
};

export default DetailPage;
