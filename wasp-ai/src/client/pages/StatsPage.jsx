import { useState, useMemo } from "react";
import getStats from "@wasp/queries/getStats";
import { useQuery } from "@wasp/queries";
import { Link } from "react-router-dom";
import { Color, availableColors } from "../components/Color";
import { format } from "timeago.js";
import { StatusPill } from "../components/StatusPill";
import { BarChart } from "../components/BarChart";
import ParentSize from "@visx/responsive/lib/components/ParentSize";
import { poolOfExampleIdeas } from "../examples";

export function Stats() {
  const [filterOutExampleApps, setFilterOutExampleApps] = useState(true);

  const { data: stats, isLoading, error } = useQuery(getStats);

  function getColorValue(colorName) {
    return availableColors.find((color) => color.name === colorName).color;
  }

  function getStatusName(status) {
    switch (status) {
      case "in-progress":
        return "inProgress";
      case "success":
        return "success";
      case "failure":
        return "error";
      default:
        return "idle";
    }
  }

  function getStatusText(status) {
    switch (status) {
      case "in-progress":
        return "In progress";
      case "success":
        return "Success";
      case "failure":
        return "Error";
      default:
        return "Idle";
    }
  }

  const filteredStats = useMemo(
    () =>
      stats
        ? stats.projects.filter((stat) => {
            if (filterOutExampleApps) {
              return !poolOfExampleIdeas.some(
                (example) =>
                  example.name === stat.name &&
                  example.description === stat.description
              );
            } else {
              return true;
            }
          })
        : [],
    [stats, filterOutExampleApps]
  );

  return (
    <div className="big-box">
      <h1 className="text-3xl font-semibold text-slate-800 mb-4">Stats</h1>

      {isLoading && <p>Loading...</p>}

      {error && <p>Error: {error.message}</p>}

      {stats && (
        <>
          <div style={{ height: 300, width: "100%" }} className="mb-4">
            <ParentSize>
              {({ width, height }) => (
                <BarChart
                  projects={filteredStats}
                  width={width}
                  height={height}
                />
              )}
            </ParentSize>
          </div>

          <div className="py-2 flex justify-between items-center">
            <div class="flex items-center mb-4">
              <input
                id="default-checkbox"
                type="checkbox"
                checked={filterOutExampleApps}
                onChange={(event) =>
                  setFilterOutExampleApps(event.target.checked)
                }
                className="w-4 h-4 text-sky-600 bg-gray-100 border-gray-300 rounded focus:ring-sky-500"
              />
              <label
                for="default-checkbox"
                class="ml-2 text-sm font-medium text-gray-900"
              >
                Filter out example apps
              </label>
            </div>
            <p className="text-sm text-slate-500">
              Number of displayed apps: {filteredStats.length}
            </p>
          </div>

          <div className="relative overflow-x-auto shadow-md sm:rounded-lg">
            <table className="w-full text-sm text-left text-slate-500">
              <thead className="text-xs text-slate-700 uppercase bg-gray-50">
                <tr>
                  <th scope="col" className="px-6 py-3">
                    App Name
                  </th>
                  <th scope="col" className="px-6 py-3">
                    Status
                  </th>
                  <th scope="col" className="px-6 py-3">
                    Created At
                  </th>
                  <th scope="col" className="px-6 py-3"></th>
                </tr>
              </thead>
              <tbody>
                {filteredStats.map((stat) => (
                  <tr className="bg-white border-b" key={stat.id}>
                    <th
                      scope="row"
                      className="px-6 py-4 font-medium text-gray-900 whitespace-nowrap flex items-center gap-2"
                      title={stat.description}
                    >
                      <Color value={getColorValue(stat.primaryColor)} />{" "}
                      {stat.name}
                    </th>
                    <td className="px-6 py-4">
                      <StatusPill status={getStatusName(stat.status)} sm>
                        {getStatusText(stat.status)}
                      </StatusPill>
                    </td>
                    <td
                      className="px-6 py-4"
                      title={`${stat.createdAt.toLocaleDateString()} ${stat.createdAt.toLocaleTimeString()}`}
                    >
                      {format(stat.createdAt)}
                    </td>
                    <td className="px-6 py-4">
                      <Link
                        to={`/result/${stat.id}`}
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
        </>
      )}
    </div>
  );
}
