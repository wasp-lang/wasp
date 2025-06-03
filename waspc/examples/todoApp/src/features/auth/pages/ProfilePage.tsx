import { useEffect } from "react";
import { type AuthUser } from "wasp/auth";
import { api } from "wasp/client/api";
import { cn } from "../../../cn";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { HookStatus } from "../components/HookStatus";

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
      <div className="grid grid-cols-1 gap-4">
        <div className="card" data-testid="user-profile">
          <h2 className="text-xl font-semibold text-gray-900 mb-4 flex items-center">
            <span className="text-2xl mr-3">👤</span>
            User Authentication
          </h2>
          <div className="space-y-3">
            <div className="flex items-center justify-between">
              <span className="text-gray-600">User ID:</span>
              <strong className="text-gray-900" data-testid="user-id">
                {user.getFirstProviderUserId()}
              </strong>
            </div>
            <div className="flex items-center justify-between">
              <span className="text-gray-600">Email Status:</span>
              <span
                className={cn(
                  "inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium",
                  user.identities.email?.isEmailVerified
                    ? "bg-green-100 text-green-800 border border-green-200"
                    : "bg-amber-100 text-amber-800 border border-amber-200",
                )}
                data-testid="email-status"
              >
                {user.identities.email?.isEmailVerified
                  ? "Verified"
                  : "Unverified"}
              </span>
            </div>
          </div>
        </div>

        <div className="card">
          <h2 className="text-xl font-semibold text-gray-900 mb-4 flex items-center">
            <span className="text-2xl mr-3">🔧</span>
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
    </FeatureContainer>
  );
};
