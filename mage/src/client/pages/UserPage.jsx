import { useState } from "react";
import { FiLogOut } from "react-icons/fi";
import { PiDownloadDuotone, PiSealWarningDuotone } from "react-icons/pi";
import { format } from "timeago.js";

import { logout } from "wasp/client/auth";
import {
  deleteMyself,
  getProjectsByUser,
  useQuery,
} from "wasp/client/operations";
import { Link } from "wasp/client/router";

import { Color } from "../components/Color";
import { MyDialog } from "../components/Dialog";
import { Header, HomeButton } from "../components/Header";
import { StatusPill } from "../components/StatusPill";
import {
  getTailwindClassNameForProjectBrandColor,
  getTailwindClassNameForProjectStatus,
  projectStatusToDisplayableText,
} from "../project/utils";

export function UserPage({ user }) {
  const [isDeleteUserModalOpen, setIsDeleteUserModalOpen] = useState(false);
  const { data: projects, isLoading, error } = useQuery(getProjectsByUser);

  if (error) return "Error loading projects.";

  return (
    <div className="container">
      <Header>
        <HomeButton />
      </Header>

      <DeleteUserModal
        isOpen={isDeleteUserModalOpen}
        setIsOpen={setIsDeleteUserModalOpen}
        deleteUser={deleteMyself}
      />

      <div className="big-box">
        <div className="flex items-center justify-between pb-6 pl-1">
          <p className="mr-2 whitespace-nowrap text-gray-700">
            <span className="font-semibold"> {user.username}</span>'s apps
          </p>
          <button className="group relative mr-1" onClick={() => logout()}>
            <FiLogOut className="h-5 w-5 text-slate-600" />
            <div className="invisible absolute bottom-100 left-1/2 -translate-x-1/2 translate-y-1 transform rounded-sm bg-slate-600 px-4 py-1 text-center text-xs whitespace-nowrap text-white opacity-0 transition-all duration-275 ease-in-out group-hover:visible group-hover:opacity-100">
              Log out
            </div>
          </button>
        </div>
        {isLoading ? (
          "Loading..."
        ) : (
          <div className="overflow-x-auto shadow-md sm:rounded-lg">
            <UserProjectsTable projects={projects} />
          </div>
        )}
      </div>
      <div className="flex justify-end px-10 pt-8">
        <button
          onClick={() => setIsDeleteUserModalOpen(true)}
          className="text-xs text-gray-500 hover:underline"
        >
          *I want to delete my account.
        </button>
      </div>
    </div>
  );
}

function UserProjectsTable({ projects }) {
  return (
    <table className="w-full text-left text-sm text-slate-500">
      <thead className="bg-gray-50 text-xs text-slate-700 uppercase">
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
        {!!projects && projects?.length > 0 ? (
          projects.map((project) => (
            <tr className="border-t bg-white" key={project.id}>
              <th
                scope="row"
                className="flex items-center gap-2 px-6 py-4 font-medium whitespace-nowrap text-gray-900"
              >
                <Color
                  value={getTailwindClassNameForProjectBrandColor(
                    project.primaryColor,
                  )}
                />{" "}
                <span
                  title={project.description}
                  className="max-w-[250px] overflow-hidden text-ellipsis"
                >
                  {project.name}
                </span>{" "}
                <span className="flex gap-1">
                  {project.zipDownloadedAt && (
                    <span
                      title={`Downloaded ${format(project.zipDownloadedAt)}`}
                      className="flex h-5 w-5 items-center justify-center rounded-full border border-sky-200 bg-sky-100 text-sky-800"
                    >
                      <PiDownloadDuotone className="h-3 w-3" />
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
          ))
        ) : (
          <tr className="border-t bg-white">
            <td colSpan={5} className="py-4 text-center">
              you have not generated any apps yet.
            </td>
          </tr>
        )}
      </tbody>
    </table>
  );
}

function DeleteUserModal({ isOpen, setIsOpen, deleteUser }) {
  async function deleteUserHandler() {
    await deleteUser();
    logout();
  }

  return (
    <MyDialog
      isOpen={isOpen}
      onClose={() => setIsOpen(false)}
      title={
        <div className="flex items-center gap-2">
          <PiSealWarningDuotone arie-hidden="true" /> Are You Sure You Want to
          Delete Your Account?
        </div>
      }
    >
      <div className="mt-10 space-y-10">
        <p className="px-8 text-center text-base leading-relaxed text-gray-500">
          You will lose access to your current projects and data.
        </p>
        <div className="flex items-center justify-between">
          <button
            className="rounded-md border border-transparent bg-red-600 px-4 py-2 text-base font-medium text-white shadow-xs hover:bg-red-700 focus:ring-2 focus:ring-red-500 focus:ring-offset-2 focus:outline-hidden"
            onClick={deleteUserHandler}
          >
            Delete Account
          </button>
          <button
            className="rounded-md border border-transparent bg-gray-100 px-4 py-2 text-base font-medium text-gray-700 shadow-xs hover:bg-gray-200 focus:ring-2 focus:ring-gray-500 focus:ring-offset-2 focus:outline-hidden"
            onClick={() => setIsOpen(false)}
          >
            Cancel
          </button>
        </div>
      </div>
    </MyDialog>
  );
}
