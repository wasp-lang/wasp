import { useQuery } from "@wasp/queries";
import getProjectsByUser from "@wasp/queries/getProjectsByUser";
import { Link } from "@wasp/router";
import { Color } from "../components/Color";
import { Header } from "../components/Header";
import { PiDownloadDuotone } from "react-icons/pi";
import logout from "@wasp/auth/logout";
import { FiLogOut } from "react-icons/fi";
import { format } from "timeago.js";
import { StatusPill } from "../components/StatusPill";
import { getColorValue, getStatusName, getStatusText } from "../stats/stats";
import { HomeButton } from "../components/Header";

export function UserPage({ user }) {
  const { data: projects, isLoading, error } = useQuery(getProjectsByUser);

  if (isLoading) return "Loading Projects...";
  if (error) return "Error loading projects.";

  return (
    <div className="container">
      <Header>
        <HomeButton />
      </Header>
      <div className="big-box">
        <div className="flex items-center justify-between pb-6 pl-1">
          <p className="text-gray-700 mr-2 whitespace-nowrap">
            <span className="font-semibold"> {user.username}</span>'s apps
          </p>
          <button className="relative group mr-1" onClick={() => logout()}>
            <FiLogOut className="w-5 h-5 text-slate-600" />
            <div className="absolute text-center whitespace-nowrap bg-slate-600 text-white text-xs rounded py-1 px-4 bottom-100 left-1/2 transform -translate-x-1/2 translate-y-1 opacity-0 invisible group-hover:opacity-100 group-hover:visible transition-all ease-in-out duration-275">
              Log out
            </div>
          </button>
        </div>
        <div className="sm:rounded-lg shadow-md overflow-x-auto ">
          <UserTable projects={projects} />
        </div>
      </div>
    </div>
  );
}

function UserTable({ projects }) {
  return (
    <table className=" w-full text-sm text-left text-slate-500">
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
        {projects?.length > 0 &&
          projects.map((project) => (
            <tr className="bg-white border-t" key={project.id}>
              <th scope="row" className="px-6 py-4 font-medium text-gray-900 whitespace-nowrap flex items-center gap-2">
                <Color value={getColorValue(project.primaryColor)} />{" "}
                <span title={project.description} className="max-w-[250px] overflow-hidden overflow-ellipsis">
                  {project.name}
                </span>{" "}
                <span className="flex gap-1">
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
  );
}