import { useEffect } from "react";
import { type AuthUser } from "wasp/auth";
import { api } from "wasp/client/api";
import { cn } from "../../../cn";
import { FeatureContainer } from "../../../components/FeatureContainer";

async function fetchCustomRoute() {
  const res = await api.get("/foo/bar");
  console.log(res.data);
}

export const ProfilePage = ({ user }: { user: AuthUser }) => {
  useEffect(() => {
    fetchCustomRoute();
  }, []);

  return (
    <FeatureContainer>
      <div className="space-y-4">
        <h2 className="feature-title">User Profile</h2>
        <div className="grid grid-cols-1 gap-4 xl:grid-cols-2">
          <div className="card" data-testid="user-profile">
            <h2 className="mb-4 flex items-center text-xl font-semibold text-gray-900">
              User Authentication
            </h2>
            <div className="space-y-3">
              <SimpleCard>
                <div className="flex items-center justify-between">
                  <span className="text-gray-600">User ID:</span>
                  <strong className="text-gray-900" data-testid="user-id">
                    {user.getFirstProviderUserId()}
                  </strong>
                </div>
              </SimpleCard>
              <SimpleCard>
                <div className="flex items-center justify-between">
                  <span className="text-gray-600">Email Status:</span>
                  <span
                    className={cn(
                      "inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-medium",
                      user.identities.email?.isEmailVerified
                        ? "border border-green-200 bg-green-100 text-green-800"
                        : "border border-amber-200 bg-amber-100 text-amber-800",
                    )}
                    data-testid="email-status"
                  >
                    {user.identities.email?.isEmailVerified
                      ? "Verified"
                      : "Unverified"}
                  </span>
                </div>
              </SimpleCard>
            </div>
          </div>

          <div className="card">
            <h2 className="mb-4 flex items-center text-xl font-semibold text-gray-900">
              Hook Status
            </h2>
            <div className="space-y-3">
              <HookStatus
                hookName="onAfterSignup"
                isCalled={user.isOnAfterSignupHookCalled}
              />
              <HookStatus
                hookName="onAfterEmailVerified"
                isCalled={user.isOnAfterEmailVerifiedHookCalled}
              />
              <HookStatus
                hookName="onAfterLogin"
                isCalled={user.isOnAfterLoginHookCalled}
              />
            </div>
          </div>
        </div>
      </div>
    </FeatureContainer>
  );
};

const HookStatus = ({
  hookName,
  isCalled,
}: {
  hookName: "onAfterSignup" | "onAfterEmailVerified" | "onAfterLogin";
  isCalled: boolean;
}) => {
  return (
    <SimpleCard>
      <div
        className="flex items-center justify-between"
        data-testid={`hook-status-${hookName}`}
      >
        <span className="text-sm text-gray-600">{hookName}</span>
        <span
          className={cn(
            "inline-flex items-center rounded-full px-2.5 py-0.5 text-xs font-medium",
            isCalled && "bg-green-100 text-green-800",
            !isCalled && "bg-gray-100 text-gray-800",
          )}
          data-testid="status"
        >
          {isCalled ? "Called" : "Not Called"}
        </span>
      </div>
    </SimpleCard>
  );
};

const SimpleCard = ({ children }: React.PropsWithChildren<{}>) => {
  return <div className="rounded-lg border p-3 shadow-sm">{children}</div>;
};
