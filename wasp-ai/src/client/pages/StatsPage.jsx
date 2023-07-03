import { useMemo } from "react";
import getStats from "@wasp/queries/getStats";
import { useQuery } from "@wasp/queries";
import { Link } from "react-router-dom";
import { Color, availableColors } from "../components/Color";
import { format } from "timeago.js";
import { StatusPill } from "../components/StatusPill";
import { BarChart } from "../components/BarChart";
import ParentSize from "@visx/responsive/lib/components/ParentSize";

export function Stats() {
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

  // Visx projects throught last 24 hours time bar chart
  const projectInLast24Hours = useMemo(() => {
    if (!stats) {
      return [];
    }
    const now = new Date();
    const last24Hours = new Date(now.getTime() - 24 * 60 * 60 * 1000);
    return stats.projects.filter((project) => {
      return project.createdAt > last24Hours;
    });
  }, [stats]);

  return (
    <div className="big-box">
      <h1 className="text-3xl font-semibold text-slate-800 mb-4">Stats</h1>

      {isLoading && <p>Loading...</p>}

      {error && <p>Error: {error.message}</p>}

      {stats && (
        <>
          <div style={{ height: 300, width: "100%" }} className="mb-4">
            <ParentSize>
              {({ width, height }) => <BarChart projects={stats.projects} width={width} height={height} />}
            </ParentSize>
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
                {stats.projects.map((stat) => (
                  <tr className="bg-white border-b" key={stat.id}>
                    <th
                      scope="row"
                      className="px-6 py-4 font-medium text-gray-900 whitespace-nowrap flex items-center gap-2"
                    >
                      <Color value={getColorValue(stat.primaryColor)} />{" "}
                      {stat.name}
                    </th>
                    <td className="px-6 py-4">
                      <StatusPill status={getStatusName(stat.status)} sm>
                        {getStatusText(stat.status)}
                      </StatusPill>
                    </td>
                    <td className="px-6 py-4" title={`${stat.createdAt.toLocaleDateString()} ${stat.createdAt.toLocaleTimeString()}`}>{format(stat.createdAt)}</td>
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
