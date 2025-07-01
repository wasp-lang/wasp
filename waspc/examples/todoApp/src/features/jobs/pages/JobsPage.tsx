import { UppercaseTextRequestState } from "@prisma/client";
import { useMutation } from "@tanstack/react-query";
import { AxiosError } from "axios";
import { useState } from "react";
import {
  getTextUppercaseRequests,
  requestUppercaseText,
  useQuery,
} from "wasp/client/operations";
import { cn } from "../../../cn";
import { Alert } from "../../../components/Alert";
import { Button } from "../../../components/Button";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { Input } from "../../../components/Input";

export function JobsPage() {
  return (
    <FeatureContainer>
      <div className="space-y-4">
        <h2 className="mb-4 text-2xl font-semibold">Async Jobs</h2>

        <CreateRequestForm />
        <RequestsList />
      </div>
    </FeatureContainer>
  );
}

function CreateRequestForm() {
  const [text, setText] = useState("");

  const requestMutation = useMutation({
    mutationFn: async () => {
      if (!text.trim()) {
        return;
      }
      await requestUppercaseText({
        text,
      });
    },
    onSuccess: () => {
      setText("");
    },
  });

  async function onSubmitJob(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault();

    requestMutation.mutate();
  }

  return (
    <div className="card">
      <form className="space-y-3" onSubmit={onSubmitJob}>
        {requestMutation.isError && (
          <Alert variant="error" className="mb-2">
            {getMutationErrorMessage(requestMutation.error)}
          </Alert>
        )}

        <Input
          type="text"
          required
          value={text}
          onChange={(e) => setText(e.target.value)}
          label="Text to Uppercase"
          placeholder="Enter some text to convert..."
          disabled={requestMutation.isLoading}
          containerClassName="max-w-md"
          data-testid="jobs-payload-input"
        />

        <Button
          type="submit"
          variant="primary"
          disabled={requestMutation.isLoading || !text.trim()}
        >
          Submit Job
        </Button>
        <p className="mb-4 text-sm text-gray-500">
          There is an artificial delay of 2 seconds in the job processing to
          simulate a real-world scenario where jobs might take time to complete.
        </p>
      </form>
    </div>
  );
}

function RequestsList() {
  const { data: requests } = useQuery(getTextUppercaseRequests, null, {
    refetchInterval: 1000,
  });
  return (
    <div className="card">
      <div className="max-h-96 space-y-3 overflow-y-auto">
        {requests && requests.length > 0 ? (
          requests.map((request) => (
            <div
              key={request.id}
              className="rounded-xl border border-gray-200 bg-white p-4 shadow-sm"
              data-testid="job-request"
            >
              <div className="space-y-3">
                <div className="flex items-start justify-between gap-3">
                  <div className="flex-1">
                    <div className="mb-1 text-sm text-gray-600">Input:</div>
                    <div
                      className="break-words font-medium text-gray-900"
                      data-testid="input"
                    >
                      "{request.input}"
                    </div>
                  </div>
                  <StatusBadge state={request.state} />
                </div>

                {request.output && (
                  <div className="border-t border-gray-100 pt-3">
                    <div className="mb-1 text-sm text-gray-600">Output:</div>
                    <div
                      className="break-words font-semibold text-gray-900"
                      data-testid="output"
                    >
                      "{request.output}"
                    </div>
                  </div>
                )}
              </div>
            </div>
          ))
        ) : (
          <div className="rounded-xl p-8 text-center shadow-sm">
            <span className="text-sm">No requests found.</span>
            <p className="mt-1 text-xs text-gray-500">
              Submit your first job to see it here!
            </p>
          </div>
        )}
      </div>
    </div>
  );
}

function StatusBadge({ state }: { state: UppercaseTextRequestState }) {
  const classes = cn(
    "inline-flex items-center px-3 py-1.5 rounded-lg text-xs font-semibold border shadow-sm",
    "bg-gray-50 text-gray-700 border-gray-200",
    state === UppercaseTextRequestState.PENDING &&
      "bg-amber-50 text-amber-700 border-amber-200 animate-pulse",
    state === UppercaseTextRequestState.SUCCESS &&
      "bg-green-50 text-green-700 border-green-200",
    state === UppercaseTextRequestState.ERROR &&
      "bg-red-50 text-red-700 border-red-200",
  );

  return (
    <span className={classes} data-testid="status">
      {state.toLowerCase()}
    </span>
  );
}

function getMutationErrorMessage(error: unknown): string | undefined {
  if (error instanceof AxiosError) {
    return error.message;
  }
  return "An unexpected error occurred.";
}
