import { AxiosError } from "axios";
import { useEffect, useState } from "react";
import { api } from "wasp/client/api";
import { FeatureContainer } from "../../../components/FeatureContainer";

export function ApisPage() {
  const api1 = useCustomApi<{ msg: string }>("/foo/bar");
  const api2 = useCustomApi<{ msg: string }>("/bar/baz");
  return (
    <FeatureContainer>
      <div className="space-y-4">
        <div className="card">
          <h2 className="text-2xl font-medium mb-4">/foo/bar</h2>
          {api1.isLoading && <div className="text-gray-500">Loading...</div>}
          {api1.error && (
            <div className="text-red-500">Error: {api1.error.message}</div>
          )}
          {api1.data && <div>{api1.data.msg}</div>}
        </div>
        <div className="card">
          <h2 className="text-2xl font-medium mb-4">/bar/baz</h2>
          {api2.isLoading && <div className="text-gray-500">Loading...</div>}
          {api2.error && (
            <div className="text-red-500">Error: {api2.error.message}</div>
          )}
          {api2.data && <div>{api2.data.msg}</div>}
        </div>
      </div>
    </FeatureContainer>
  );
}

function useCustomApi<Data = unknown>(
  endpoint: string,
): {
  isLoading: boolean;
  data: Data | null;
  error: AxiosError | null;
} {
  const [isLoading, setIsLoading] = useState(true);
  const [data, setData] = useState<Data | null>(null);
  const [error, setError] = useState<AxiosError | null>(null);

  useEffect(() => {
    const abortController = new AbortController();
    api
      .get(endpoint, {
        signal: abortController.signal,
      })
      .then((response) => {
        setData(response.data);
        setError(null);
        setIsLoading(false);
      })
      .catch((err: unknown) => {
        setError(err as AxiosError);
        setData(null);
        setIsLoading(false);
      });
    return () => {
      abortController.abort();
    };
  }, []);

  return { isLoading, data, error };
}
