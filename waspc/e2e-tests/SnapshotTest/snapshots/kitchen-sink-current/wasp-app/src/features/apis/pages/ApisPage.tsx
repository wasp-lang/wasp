import { useQuery } from "@tanstack/react-query";
import { AxiosError } from "axios";
import { api } from "wasp/client/api";
import { Alert } from "../../../components/Alert";
import { FeatureContainer } from "../../../components/FeatureContainer";

export function ApisPage() {
  const authenticatedApi = useCustomApi<{ msg: string }>("/foo/bar");
  const unauthenticatedApi = useCustomApi<{ msg: string }>("/bar/baz");
  return (
    <FeatureContainer>
      <div className="space-y-4">
        <h2 className="feature-title">Custom APIs</h2>
        <div className="card" data-testid="authenticated-api">
          <h2 className="mb-2 text-xl font-medium">/foo/bar</h2>
          {authenticatedApi.isLoading && (
            <div className="text-gray-500">Loading...</div>
          )}
          {authenticatedApi.error && (
            <Alert variant="error">
              <span data-testid="error">
                Error: {authenticatedApi.error.message}
              </span>
            </Alert>
          )}
          {authenticatedApi.data && (
            <div data-testid="data">{authenticatedApi.data.msg}</div>
          )}
        </div>
        <div className="card" data-testid="unauthenticated-api">
          <h2 className="mb-2 text-xl font-medium">/bar/baz</h2>
          {unauthenticatedApi.isLoading && (
            <div className="text-gray-500">Loading...</div>
          )}
          {unauthenticatedApi.error && (
            <Alert variant="error" data-testid="error">
              <span data-testid="error">
                Error: {unauthenticatedApi.error.message}
              </span>
            </Alert>
          )}
          {unauthenticatedApi.data && (
            <div data-testid="data">{unauthenticatedApi.data.msg}</div>
          )}
        </div>
      </div>
    </FeatureContainer>
  );
}

function useCustomApi<Data = unknown>(
  endpoint: string,
): {
  isLoading: boolean;
  data: Data | undefined;
  error: AxiosError | null;
} {
  const { isLoading, data, error } = useQuery<Data, AxiosError>(
    [endpoint],
    () => api.get(endpoint).then((response) => response.data),
    {
      retry: false,
    },
  );

  return { isLoading, data, error };
}
