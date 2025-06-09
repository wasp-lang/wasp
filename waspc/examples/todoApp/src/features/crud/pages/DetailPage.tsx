import { useParams } from "react-router-dom";
import { Link } from "wasp/client/router";

import { tasks as tasksCrud } from "wasp/client/crud";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { TaskDetailView } from "../../../components/TaskDetailView";

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
    <FeatureContainer>
      <div className="space-y-4" data-testid="crud-task-details">
        <h2 className="feature-title">CRUD Task Detail Page</h2>
        <div className="tasks">
          {isLoading && <div>Loading...</div>}
          {task && (
            <div className="card">
              <TaskDetailView task={task} />
            </div>
          )}
        </div>
        <div>
          <Link to="/crud" className="link">
            Return to task list
          </Link>
        </div>
      </div>
    </FeatureContainer>
  );
};
