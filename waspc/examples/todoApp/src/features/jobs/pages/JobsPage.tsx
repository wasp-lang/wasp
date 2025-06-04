// import { UppercaseTextRequestState } from "@prisma/client";
import { useState } from "react";
import {
  getTextUppercaseRequests,
  requestUppercaseText,
  useQuery,
} from "wasp/client/operations";
import { cn } from "../../../cn";
import { Button } from "../../../components/Button";
import { FeatureContainer } from "../../../components/FeatureContainer";
import { Input } from "../../../components/Input";

export function JobsPage() {
  const { data: requests } = useQuery(getTextUppercaseRequests, null, {
    refetchInterval: 1000,
  });
  const [jobPayload, setJobPayload] = useState("");
  const [isSubmitting, setIsSubmitting] = useState(false);

  async function onSubmitJob(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault();
    if (!jobPayload || isSubmitting) {
      return;
    }

    setIsSubmitting(true);
    try {
      await requestUppercaseText({
        text: jobPayload,
      });
      setJobPayload("");
    } catch (error) {
      console.error("Failed to submit job:", error);
    } finally {
      setIsSubmitting(false);
    }
  }

  return (
    <FeatureContainer>
      <div className="space-y-4">
        <div className="card">
          <h2 className="text-xl font-semibold text-gray-900 mb-4 flex items-center">
            Text Uppercasing Job Demo
          </h2>

          <form className="space-y-3" onSubmit={onSubmitJob}>
            <Input
              type="text"
              required
              value={jobPayload}
              onChange={(e) => setJobPayload(e.target.value)}
              label="Text to Uppercase"
              placeholder="Enter some text to convert..."
              disabled={isSubmitting}
              containerClassName="max-w-md"
            />

            <Button
              type="submit"
              variant="primary"
              disabled={isSubmitting || !jobPayload.trim()}
            >
              Submit Job
            </Button>
            <p className="text-sm text-gray-500 mb-4">
              There is an artificial delay of 2 seconds in the job processing to
              simulate a real-world scenario where jobs might take time to
              complete.
            </p>
          </form>
        </div>
        <div className="card">
          <div className="space-y-3 max-h-96 overflow-y-auto">
            {requests && requests.length > 0 ? (
              requests.map((request) => (
                <div
                  key={request.id}
                  className="border border-gray-200 p-4 rounded-xl shadow-sm bg-white hover:shadow-md transition-shadow duration-200"
                >
                  <div className="space-y-3">
                    <div className="flex items-start justify-between gap-3">
                      <div className="flex-1">
                        <div className="text-sm text-gray-600 mb-1">Input:</div>
                        <div className="text-gray-900 font-medium break-words">
                          "{request.input}"
                        </div>
                      </div>
                      <StatusBadge state={request.state} />
                    </div>

                    {request.output && (
                      <div className="border-t border-gray-100 pt-3">
                        <div className="text-sm text-gray-600 mb-1">
                          Output:
                        </div>
                        <div className="text-gray-900 font-semibold break-words">
                          "{request.output}"
                        </div>
                      </div>
                    )}
                  </div>
                </div>
              ))
            ) : (
              <div className="p-8 rounded-xl shadow-sm text-center">
                <span className="text-sm">No requests found.</span>
                <p className="text-gray-500 text-xs mt-1">
                  Submit your first job to see it here!
                </p>
              </div>
            )}
          </div>
        </div>
      </div>
    </FeatureContainer>
  );
}

function StatusBadge({ state }: { state: string }) {
  const getClasses = (state: string) => {
    const baseClasses =
      "inline-flex items-center px-3 py-1.5 rounded-lg text-xs font-semibold border shadow-sm";

    switch (state) {
      case "PENDING":
        return cn(
          baseClasses,
          "bg-amber-50 text-amber-700 border-amber-200 animate-pulse",
        );
      case "SUCCESS":
        return cn(baseClasses, "bg-green-50 text-green-700 border-green-200");
      case "ERROR":
        return cn(baseClasses, "bg-red-50 text-red-700 border-red-200");
      default:
        return cn(baseClasses, "bg-gray-50 text-gray-700 border-gray-200");
    }
  };

  return <span className={getClasses(state)}>{state.toLowerCase()}</span>;
}
