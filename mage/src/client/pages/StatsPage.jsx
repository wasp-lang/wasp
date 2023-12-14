import { useState, useMemo } from "react";
import getProjects from "@wasp/queries/getProjects";
import getStats from "@wasp/queries/getStats";
import { useQuery } from "@wasp/queries";
import { Link } from "react-router-dom";
import { Color } from "../components/Color";
import { format } from "timeago.js";
import { StatusPill } from "../components/StatusPill";
import { BarChart } from "../components/BarChart";
import ParentSize from "@visx/responsive/lib/components/ParentSize";
import { exampleIdeas } from "../examples";
import logout from "@wasp/auth/logout";
import { Header } from "../components/Header";
import { PiDownloadDuotone, PiUserDuotone } from "react-icons/pi";
import { MyDropdown } from "../components/Dropdown";
import { HomeButton } from "../components/Header";
import {
  getTailwindClassNameForProjectBrandColor,
  getTailwindClassNameForProjectStatus,
  projectStatusToDisplayableText,
} from "../project/utils";

const chartTypes = [
  {
    name: "Last 24 hours",
    value: "last24Hours",
  },
  {
    name: "Last 30 days",
    value: "last30Days",
  },
];

export function Stats() {
  const [filterOutExampleApps, setFilterOutExampleApps] = useState(false);
  const [chartType, setChartType] = useState(chartTypes[0]);

  const { data: projects, isLoading, error } = useQuery(getProjects);
  const { data: stats } = useQuery(getStats, {
    filterOutExampleApps,
  });

  const logsByProjectId = useMemo(() => {
    if (!projects) {
      return {};
    }
    if (!projects.latestProjectsWithLogs) {
      return {};
    }
    return projects.latestProjectsWithLogs.reduce((acc, project) => {
      acc[project.id] = project.logs;
      return acc;
    }, {});
  }, [projects]);

  const filteredProjects = useMemo(() => {
    const filters = [];
    if (filterOutExampleApps) {
      filters.push((stat) => !exampleIdeas.some((example) => example.name === stat.name));
    }
    return projects
      ? projects.projects.filter((stat) => {
          return filters.every((filter) => filter(stat));
        })
      : [];
  }, [projects, projects?.projects, filterOutExampleApps]);

  const barChartData = useMemo(() => {
    if (!stats) {
      return [];
    }

    if (chartType.value === "last24Hours") {
      return stats.last24Hours;
    } else {
      return stats.last30Days;
    }
  }, [stats, chartType, filteredProjects]);

  if (isLoading) {
    return <p>Loading...</p>;
  }

  if (error) {
    return <p>Error: {error.message}</p>;
  }

  if (!projects || !stats) {
    return <p>Couldn't load stats</p>;
  }

  return (
    <>
      <Header>
        <HomeButton />
      </Header>
      <div className="big-box">
        <div className="flex justify-between items-center mb-4">
          <h1 className="text-3xl font-semibold text-slate-800">Stats</h1>
          <div>
            <button className="button sm" onClick={logout}>
              Logout
            </button>
          </div>
        </div>

        {projects.projects.length === 0 && (
          <p className="text-sm text-slate-500">No projects created yet.</p>
        )}

        {projects.projects.length > 0 && (
          <>
            <div className="mb-3 flex justify-between items-end">
              <div>
                <h2 className="text-xl font-semibold text-slate-800">Projects over time</h2>
              </div>
              <div className="w-1/3">
                <MyDropdown options={chartTypes} value={chartType} onChange={setChartType} />
              </div>
            </div>
            <div style={{ height: 300, width: "100%" }} className="mb-4">
              <ParentSize>
                {({ width, height }) => (
                  <BarChart
                    chartType={chartType.value}
                    data={barChartData}
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
                    onChange={(event) => setFilterOutExampleApps(event.target.checked)}
                    className="w-4 h-4 text-sky-600 bg-gray-100 border-gray-300 rounded focus:ring-sky-500"
                  />
                  <label htmlFor="filter" className="ml-2 text-sm font-medium text-gray-900">
                    Filter out example apps
                  </label>
                </div>
              </div>

              {stats && (
                <p className="text-sm text-slate-800 flex gap-2">
                  <span className="bg-slate-100 rounded-md px-2 py-1">
                    Generated: <strong className="text-slate-800">{stats.totalGenerated}</strong>
                  </span>
                  <span className="bg-slate-100 rounded-md px-2 py-1">
                    Downloaded:{" "}
                    <strong className="text-slate-800">{`${stats.totalDownloaded} (${stats.downloadedPercentage}%)`}</strong>
                  </span>
                </p>
              )}
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
                  {filteredProjects.map((project) => (
                    <tr className="bg-white border-b" key={project.id}>
                      <th
                        scope="row"
                        className="px-6 py-4 font-medium text-gray-900 whitespace-nowrap flex items-center gap-2"
                      >
                        <Color
                          value={getTailwindClassNameForProjectBrandColor(project.primaryColor)}
                        />{" "}
                        <span className="max-w-[250px] overflow-hidden overflow-ellipsis">
                          {project.name}
                        </span>{" "}
                        <span className="flex gap-1">
                          {project.user && (
                            <span
                              title={project.user.email}
                              className="w-5 h-5 bg-yellow-100 rounded-full flex items-center justify-center text-yellow-800 border border-yellow-200"
                            >
                              <PiUserDuotone className="w-3 h-3" />
                            </span>
                          )}
                          {project.zipDownloadedAt && (
                            <span
                              title={`Downloaded ${format(project.zipDownloadedAt)}`}
                              className="w-5 h-5 bg-sky-100 rounded-full flex items-center justify-center text-sky-800 border border-sky-200"
                            >
                              <PiDownloadDuotone className="w-3 h-3" />
                            </span>
                          )}
                        </span>
                      </th>
                      <td className="px-6 py-4">
                        <StatusPill
                          status={getTailwindClassNameForProjectStatus(project.status)}
                          sm
                        >
                          {projectStatusToDisplayableText(project.status)}
                        </StatusPill>
                      </td>
                      <td
                        className="px-6 py-4"
                        title={`${project.createdAt.toLocaleDateString()} ${project.createdAt.toLocaleTimeString()}`}
                      >
                        {format(project.createdAt)}
                      </td>
                      <td className="px-6 py-4">
                        {getWaitingInQueueDuration(project, logsByProjectId)} &rarr;{" "}
                        {getDuration(project, logsByProjectId)}
                      </td>
                      <td className={`px-6 py-4 creativity-${project.creativityLevel}`}>
                        {project.creativityLevel}
                      </td>
                      <td className="px-6 py-4">
                        <Link
                          to={`/result/${project.id}`}
                          className="font-medium text-sky-600 hover:underline"
                        >
                          View the app &rarr;
                        </Link>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
              <div className="relative px-6 py-3 bg-gray-50 text-sm text-slate-500 text-center">
                Showing only the latest 1000 projects
              </div>
            </div>
          </>
        )}
      </div>
    </>
  );
}

function getFormattedDiff(start, end) {
  const diff = (end - start) / 1000;
  const minutes = Math.round(diff / 60);
  const remainingSeconds = Math.round(diff % 60);
  return `${minutes}m ${remainingSeconds}s`;
}

function getDuration(stat, logsByProjectId) {
  if (!logsByProjectId[stat.id]) {
    return "-";
  }
  const logs = logsByProjectId[stat.id];
  if (logs.length < 2) {
    return "-";
  }
  const start = logs[logs.length - 1].createdAt;
  const end = logs[0].createdAt;
  return getFormattedDiff(start, end);
}

function getWaitingInQueueDuration(stat, logsByProjectId) {
  if (!logsByProjectId[stat.id]) {
    return "-";
  }
  const logs = logsByProjectId[stat.id];
  if (logs.length < 2) {
    return "-";
  }
  const start = stat.createdAt;
  const end = logs[logs.length - 1].createdAt;
  return getFormattedDiff(start, end);
}
