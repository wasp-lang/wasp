import { useState, useMemo } from "react";
import getStats from "@wasp/queries/getStats";
import { useQuery } from "@wasp/queries";
import { Link } from "react-router-dom";
import { Color, availableColors } from "../components/Color";
import { format } from "timeago.js";
import { StatusPill } from "../components/StatusPill";
import { BarChart } from "../components/BarChart";
import ParentSize from "@visx/responsive/lib/components/ParentSize";
import { exampleIdeas } from "../examples";
import logout from "@wasp/auth/logout";
import { WaspIcon } from "../components/WaspIcon";
import { Header } from "../components/Header";

export function Stats() {
  const [filterOutExampleApps, setFilterOutExampleApps] = useState(false);
  const [filterOutKnownUsers, setFilterOutKnownUsers] = useState(false);

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
      case "cancelled":
        return "cancelled";
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
      case "cancelled":
        return "Cancelled";
      case "pending":
        return "Pending";
      default:
        return "Unknown";
    }
  }

  const filteredStats = useMemo(() => {
    const filters = [];
    if (filterOutExampleApps) {
      filters.push(
        (stat) =>
          !exampleIdeas.some(
            (example) =>
              example.name === stat.name &&
              example.description === stat.description
          )
      );
    }
    if (filterOutKnownUsers) {
      filters.push((stat) => !stat.user);
    }
    return stats
      ? stats.projects.filter((stat) => {
          return filters.every((filter) => filter(stat));
        })
      : [];
  }, [stats, stats?.projects, filterOutExampleApps, filterOutKnownUsers]);

  function getFormattedDiff(start, end) {
    const diff = (end - start) / 1000;
    const minutes = Math.round(diff / 60);
    const remainingSeconds = Math.round(diff % 60);
    return `${minutes}m ${remainingSeconds}s`;
  }

  function getDuration(stat) {
    if (stat.logs.length < 2) {
      return "-";
    }
    const start = stat.logs[stat.logs.length - 1].createdAt;
    const end = stat.logs[0].createdAt;
    return getFormattedDiff(start, end);
  }

  function getWaitingInQueueDuration(stat) {
    if (stat.logs.length < 2) {
      return "-";
    }
    const start = stat.createdAt;
    const end = stat.logs[stat.logs.length - 1].createdAt;
    return getFormattedDiff(start, end);
  }

  return (
    <>
      <Header />
      <div className="big-box">
        <div className="flex justify-between items-center mb-4">
          <h1 className="text-3xl font-semibold text-slate-800">Stats</h1>
          <div>
            <button className="button sm" onClick={logout}>
              Logout
            </button>
          </div>
        </div>

        {isLoading && <p>Loading...</p>}

        {error && <p>Error: {error.message}</p>}

        {stats && stats.projects.length === 0 && (
          <p className="text-sm text-slate-500">No projects created yet.</p>
        )}

        {stats && stats.projects.length > 0 && (
          <>
            <p className="text-sm text-slate-500 mb-2">
              Number of projects created in the last 24 hours:{" "}
            </p>
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
              <div className="flex gap-3">
                <div className="flex items-center mb-4">
                  <input
                    id="filter"
                    type="checkbox"
                    checked={filterOutExampleApps}
                    onChange={(event) =>
                      setFilterOutExampleApps(event.target.checked)
                    }
                    className="w-4 h-4 text-sky-600 bg-gray-100 border-gray-300 rounded focus:ring-sky-500"
                  />
                  <label
                    htmlFor="filter"
                    className="ml-2 text-sm font-medium text-gray-900"
                  >
                    Filter out example apps
                  </label>
                </div>
                <div className="flex items-center mb-4">
                  <input
                    id="default-checkbox"
                    type="checkbox"
                    checked={filterOutKnownUsers}
                    onChange={(event) =>
                      setFilterOutKnownUsers(event.target.checked)
                    }
                    className="w-4 h-4 text-sky-600 bg-gray-100 border-gray-300 rounded focus:ring-sky-500"
                  />
                  <label
                    htmlFor="default-checkbox"
                    className="ml-2 text-sm font-medium text-gray-900"
                  >
                    Filter out known users
                  </label>
                </div>
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
                    <th scope="col" className="px-6 py-3">
                      Time in Queue &rarr; Build
                    </th>
                    <th scope="col" className="px-6 py-3">
                      Creativity lvl
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
                      >
                        <Color value={getColorValue(stat.primaryColor)} />{" "}
                        <span title={stat.description}>{stat.name}</span>{" "}
                        {stat.user && (
                          <span
                            className="text-slate-300"
                            title={stat.user.email}
                          >
                            <WaspIcon className="w-5 h-5" />
                          </span>
                        )}
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
                        {getWaitingInQueueDuration(stat)} &rarr;{" "}
                        {getDuration(stat)}
                      </td>
                      <td className={`px-6 py-4 creativity-${stat.creativityLevel}`}>
                        {stat.creativityLevel}
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
    </>
  );
}
