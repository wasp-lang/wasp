import { getTeapot, useQuery } from "wasp/client/operations";
import { HttpError } from "wasp/errors";
import { FeatureContainer } from "../../../components/FeatureContainer";

export const ErrorsPage = () => {
  const { error } = useQuery(getTeapot, undefined, { retry: false });

  return (
    <FeatureContainer>
      <div className="space-y-4">
        <h2 className="feature-title">Errors</h2>
        {error instanceof HttpError && (
          <dl className="card" data-testid="http-error">
            <dt>Status</dt>
            <dd data-testid="status">{error.statusCode}</dd>
            <dt>Message</dt>
            <dd data-testid="message">{error.message}</dd>
            <dt>Reason</dt>
            <dd data-testid="reason">{String(error.data?.reason)}</dd>
          </dl>
        )}
      </div>
    </FeatureContainer>
  );
};
