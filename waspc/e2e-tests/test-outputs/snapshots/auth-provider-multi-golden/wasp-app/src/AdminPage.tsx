import { getAdminReport, useQuery } from "wasp/client/operations";

/**
 * Reachable only with a wasp-minted session (`authRequired: ["wasp"]` on the
 * route). A Clerk-authenticated user sees Wasp's access-denied message
 * instead of this component; an unauthenticated visitor is redirected to the
 * login page. The query underneath carries the same restriction, so the page
 * check is UX and the server check is the gate.
 */
export function AdminPage() {
  const { data: report, error } = useQuery(getAdminReport);

  return (
    <main
      style={{ maxWidth: 480, margin: "3rem auto", fontFamily: "system-ui" }}
    >
      <h1>Admin report</h1>
      {error ? <p>Failed to load the report: {error.message}</p> : null}
      {report ? (
        <p>
          {report.taskCount} tasks across all users, read via a session minted
          by <code>{report.sessionProviderId}</code>.
        </p>
      ) : null}
    </main>
  );
}
