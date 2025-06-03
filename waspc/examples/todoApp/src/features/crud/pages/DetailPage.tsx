import { useParams } from "react-router-dom";
import { Link } from "wasp/client/router";

import { tasks as tasksCrud } from "wasp/client/crud";
import { FeatureContainer } from "../../../components/FeatureContainer";

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
      <div className="space-y-4">
        <h1 className="text-2xl font-medium">CRUD Task Detail Page</h1>
        <div className="tasks">
          {isLoading && <div>Loading...</div>}
          {task && (
            <div className="card">
              <code className="whitespace-pre-wrap break-words text-sm text-gray-800">
                {JSON.stringify(task, null, 2)}
              </code>
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
