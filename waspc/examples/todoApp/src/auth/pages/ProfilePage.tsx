import { useEffect, useRef } from "react";
import { type AuthUser } from "wasp/auth";
import { api } from "wasp/client/api";
import { Link, routes } from "wasp/client/router";
import { SimplePageContainer } from "../../components/SimplePageContainer";

async function fetchCustomRoute() {
  const res = await api.get("/foo/bar");
  console.log(res.data);
}

export const ProfilePage = ({ user }: { user: AuthUser }) => {
  const inputRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    fetchCustomRoute();
  }, []);

  return (
    <SimplePageContainer>
      <h2 className="mt-4 mb-2 font-bold text-xl">User Auth Fields Demo</h2>
      <div>
        Hello <strong>{user.getFirstProviderUserId()}</strong>! Your status is{" "}
        <strong>
          {user.identities.email?.isEmailVerified ? "verfied" : "unverified"}
        </strong>
        .
      </div>
      <div>
        Value of <code>user.isOnAfterSignupHookCalled</code> is{" "}
        <strong>{user.isOnAfterSignupHookCalled ? "true" : "false"}</strong>.
      </div>
      <div>
        Value of <code>user.isOnAfterEmailVerifiedHookCalled</code> is{" "}
        <strong>
          {user.isOnAfterEmailVerifiedHookCalled ? "true" : "false"}
        </strong>
        .
      </div>
      <div>
        Value of <code>user.isOnAfterLoginHookCalled</code> is{" "}
        <strong>{user.isOnAfterLoginHookCalled ? "true" : "false"}</strong>.
      </div>
      <div>
        First provider ID: <strong>{user.getFirstProviderUserId()}</strong>
      </div>
      <h2 className="mt-4 mb-2 font-bold text-xl">Links Demo</h2>
      <Link to="/task/:id" params={{ id: 3 }}>
        Task 3
      </Link>
      <p>
        Route is{" "}
        {routes.TaskRoute.build({
          params: { id: 5 },
          search: { google: "true" },
          hash: "Miho",
        })}
      </p>
    </SimplePageContainer>
  );
};
