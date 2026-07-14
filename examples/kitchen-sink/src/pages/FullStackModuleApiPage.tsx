import { requestModuleJob } from "@kitchen-sink/module/moduleJobClient";
import { useState } from "react";
import { FeatureContainer } from "../components/FeatureContainer";

export function FullStackModuleApiPage() {
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [jobId, setJobId] = useState<string | null>(null);
  const [error, setError] = useState<string | null>(null);

  async function handleSubmit() {
    setIsSubmitting(true);
    setJobId(null);
    setError(null);

    try {
      const response = await requestModuleJob("host-page", "/fsm");
      setJobId(response.jobId);
    } catch (error) {
      setError(error instanceof Error ? error.message : String(error));
    } finally {
      setIsSubmitting(false);
    }
  }

  return (
    <FeatureContainer>
      <div className="space-y-4">
        <h2 className="feature-title">Module API from a host page</h2>
        <div className="card max-w-2xl space-y-4">
          <p>
            This host-owned page imports the module's client helper, which calls
            the module's API and submits its PgBoss job.
          </p>
          <button
            type="button"
            className="rounded-md bg-yellow-500 px-4 py-2 font-medium text-black disabled:cursor-not-allowed disabled:opacity-50"
            disabled={isSubmitting}
            onClick={handleSubmit}
          >
            {isSubmitting ? "Submitting..." : "Submit module job"}
          </button>
          {jobId && (
            <p data-testid="host-module-job-id">Submitted job: {jobId}</p>
          )}
          {error && (
            <p className="text-red-600">Failed to submit job: {error}</p>
          )}
        </div>
      </div>
    </FeatureContainer>
  );
}
