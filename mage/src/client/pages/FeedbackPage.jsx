import { Link } from "react-router";
import { format } from "timeago.js";

import { getFeedback, useQuery } from "wasp/client/operations";

export function Feedback() {
  const { data: feedback, isLoading, error } = useQuery(getFeedback);

  return (
    <>
      {isLoading && <p>Loading...</p>}

      {error && <p>Error: {error.message}</p>}

      {feedback && feedback.feedbackEntries.length === 0 && (
        <p className="text-sm text-slate-500">No feedback yet.</p>
      )}

      {feedback && feedback.feedbackEntries.length > 0 && (
        <div className="relative overflow-x-auto shadow-md sm:rounded-lg">
          <table className="w-full text-left text-sm text-slate-500">
            <thead className="bg-gray-50 text-xs text-slate-700 uppercase">
              <tr>
                <th scope="col" className="px-6 py-3">
                  App Name
                </th>
                <th scope="col" className="px-6 py-3">
                  Created At
                </th>
                <th scope="col" className="px-6 py-3">
                  Score
                </th>
                <th scope="col" className="px-6 py-3">
                  Message
                </th>
                <th scope="col" className="px-6 py-3"></th>
              </tr>
            </thead>
            <tbody>
              {feedback.feedbackEntries.map((entry, idx) => (
                <tr className="border-b bg-white" key={idx}>
                  <th
                    scope="row"
                    className="flex items-center gap-2 px-6 py-4 font-medium whitespace-nowrap text-gray-900"
                  >
                    <span title={entry.project.description}>
                      {entry.project.name}
                    </span>{" "}
                  </th>
                  <td
                    className="px-6 py-4"
                    title={`${entry.createdAt.toLocaleDateString()} ${entry.createdAt.toLocaleTimeString()}`}
                  >
                    {format(entry.createdAt)}
                  </td>
                  <td
                    scope="row"
                    className="flex items-center gap-2 px-6 py-4 font-medium whitespace-nowrap text-gray-900"
                  >
                    {entry.score}
                  </td>
                  <td className="px-6 py-4">{entry.message}</td>
                  <td className="px-6 py-4">
                    <Link
                      to={`/result/${entry.projectId}`}
                      className="font-medium text-sky-600 hover:underline"
                    >
                      View the app &rarr;
                    </Link>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </>
  );
}
