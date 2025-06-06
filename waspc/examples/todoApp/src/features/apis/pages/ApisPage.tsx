import { useQuery } from "@tanstack/react-query";
import { AxiosError } from "axios";
import { api } from "wasp/client/api";
import { FeatureContainer } from "../../../components/FeatureContainer";

export function ApisPage() {
  const authenticatedApi = useCustomApi<{ msg: string }>("/foo/bar");
  const unauthenticatedApi = useCustomApi<{ msg: string }>("/bar/baz");
  return (
    <FeatureContainer>
      <div className="space-y-4">
        <div className="card" data-testid="authenticated-api">
          <h2 className="text-2xl font-medium mb-4">/foo/bar</h2>
          {authenticatedApi.isLoading && (
            <div className="text-gray-500">Loading...</div>
          )}
          {authenticatedApi.error && (
            <div className="text-red-500" data-testid="error">
              Error: {authenticatedApi.error.message}
            </div>
          )}
          {authenticatedApi.data && (
            <div data-testid="data">{authenticatedApi.data.msg}</div>
          )}
        </div>
        <div className="card" data-testid="unauthenticated-api">
          <h2 className="text-2xl font-medium mb-4">/bar/baz</h2>
          {unauthenticatedApi.isLoading && (
            <div className="text-gray-500">Loading...</div>
          )}
          {unauthenticatedApi.error && (
            <div className="text-red-500" data-testid="error">
              Error: {unauthenticatedApi.error.message}
            </div>
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
