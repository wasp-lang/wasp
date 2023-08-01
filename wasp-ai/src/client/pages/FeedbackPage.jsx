import { useState, useMemo } from "react";
import getFeedback from "@wasp/queries/getFeedback";
import { useQuery } from "@wasp/queries";
import { Link } from "react-router-dom";
import { format } from "timeago.js";

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
          <table className="w-full text-sm text-left text-slate-500">
            <thead className="text-xs text-slate-700 uppercase bg-gray-50">
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
                <tr className="bg-white border-b" key={idx}>
                  <th
                    scope="row"
                    className="px-6 py-4 font-medium text-gray-900 whitespace-nowrap flex items-center gap-2"
                  >
                    <span title={entry.project.description}>{entry.project.name}</span>{" "}
                  </th>
                  <td
                    className="px-6 py-4"
                    title={`${entry.createdAt.toLocaleDateString()} ${entry.createdAt.toLocaleTimeString()}`}
                  >
                    {format(entry.createdAt)}
                  </td>
                  <td
                    scope="row"
                    className="px-6 py-4 font-medium text-gray-900 whitespace-nowrap flex items-center gap-2"
                  >
                    {entry.score}
                  </td>
                  <td className="px-6 py-4">
                    {entry.message}
                  </td>
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
  )
}