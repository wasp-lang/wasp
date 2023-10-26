import { useMemo } from "react";
import { useQuery } from "@wasp/queries";
import getProjectsByUser from "@wasp/queries/getProjectsByUser";
import getStats from "@wasp/queries/getStats";
import { Link } from "@wasp/router";
import { Color, availableColors } from "../components/Color";
import { Header } from "../components/Header";
import { PiDownloadDuotone } from "react-icons/pi";
import { format } from "timeago.js";
import { StatusPill } from "../components/StatusPill";
import logout from "@wasp/auth/logout";
import { WaspIcon } from "../components/WaspIcon";

export function UserPage({ user }) {

  const { data: projects, isLoading, error } = useQuery(getProjectsByUser );

  if (isLoading) "Loading Projects...";
  if (error) "Error loading projects.";

  return (
    <>
    <Header/>
      <div className="flex items-center p-6">
        <p className="text-gray-700 uppercase mr-2">
          🧙 Hi, <span className="font-semibold">{user.username} </span>
        </p>
      </div>

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
              Creativity lvl
            </th>
            <th scope="col" className="px-6 py-3"></th>
          </tr>
        </thead>
        <tbody>
          {projects?.length &&
            projects.map((project) => (
              <tr className="bg-white border-b" key={project.id}>
                <th
                  scope="row"
                  className="px-6 py-4 font-medium text-gray-900 whitespace-nowrap flex items-center gap-2"
                >
                  <Color value={getColorValue(project.primaryColor)} />{" "}
                  <span title={project.description} className="max-w-[250px] overflow-hidden overflow-ellipsis">
                    {project.name}
                  </span>{" "}
                  <span className="flex gap-1">
                    {project.user && (
                      <span title={project.user.email}>
                        <WaspIcon className="w-5 h-5" />
                      </span>
                    )}
                    {project.zipDownloadedAt && (
                      <span
                        title={`Downlaoded ${format(project.zipDownloadedAt)}`}
                        className="w-5 h-5 bg-sky-100 rounded-full flex items-center justify-center text-sky-800 border border-sky-200"
                      >
                        <PiDownloadDuotone className="w-3 h-3" />
                      </span>
                    )}
                  </span>
                </th>
                <td className="px-6 py-4">
                  <StatusPill status={getStatusName(project.status)} sm>
                    {getStatusText(project.status)}
                  </StatusPill>
                </td>
                <td
                  className="px-6 py-4"
                  title={`${project.createdAt.toLocaleDateString()} ${project.createdAt.toLocaleTimeString()}`}
                >
                  {format(project.createdAt)}
                </td>
                <td className={`px-6 py-4 creativity-${project.creativityLevel}`}>{project.creativityLevel}</td>
                <td className="px-6 py-4">
                  <Link to={`/result/${project.id}`} className="font-medium text-sky-600 hover:underline">
                    View the app &rarr;
                  </Link>
                </td>
              </tr>
            ))}
        </tbody>
      </table>

      <div className="flex justify-end w-full">
        <button className="button mt-6" onClick={() => logout()}>
          Log out
        </button>
      </div>
    </>
  );
}

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
