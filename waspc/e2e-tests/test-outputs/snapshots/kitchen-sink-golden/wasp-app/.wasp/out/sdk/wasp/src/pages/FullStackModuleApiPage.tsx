import {
  pingModuleApi,
  requestModuleJob,
} from "@kitchen-sink/module/moduleApiClient";
import {
  getModuleApiPrefix,
  getModuleJobApiPath,
  getModulePingApiPath,
  MODULE_API_HEADER_NAME,
} from "@kitchen-sink/module/moduleApiContract";
import { useState } from "react";
import { Alert } from "../components/Alert";
import { Button } from "../components/Button";
import { FeatureContainer } from "../components/FeatureContainer";

const MODULE_PREFIX = "/fsm";

export function FullStackModuleApiPage() {
  const [isPinging, setIsPinging] = useState(false);
  const [moduleApiHeader, setModuleApiHeader] = useState<string | null>(null);
  const [pingError, setPingError] = useState<string | null>(null);
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [jobId, setJobId] = useState<string | null>(null);
  const [jobError, setJobError] = useState<string | null>(null);

  async function handlePing() {
    setIsPinging(true);
    setModuleApiHeader(null);
    setPingError(null);

    try {
      const response = await pingModuleApi(MODULE_PREFIX);
      setModuleApiHeader(response.moduleApiHeader);
    } catch (error) {
      setPingError(error instanceof Error ? error.message : String(error));
    } finally {
      setIsPinging(false);
    }
  }

  async function handleSubmitJob() {
    setIsSubmitting(true);
    setJobId(null);
    setJobError(null);

    try {
      const response = await requestModuleJob("host-page", MODULE_PREFIX);
      setJobId(response.jobId);
    } catch (error) {
      setJobError(error instanceof Error ? error.message : String(error));
    } finally {
      setIsSubmitting(false);
    }
  }

  return (
    <FeatureContainer>
      <div className="space-y-4">
        <h2 className="feature-title">Module API from the host app</h2>
        <p>
          Calls the module's API without auth. The module's own UI lives at{" "}
          <code>{MODULE_PREFIX}</code>.
        </p>
        <div className="card max-w-2xl space-y-4">
          <div>
            <p className="text-xs font-semibold tracking-wide text-gray-500 uppercase">
              api + apiNamespace
            </p>
            <h2 className="text-xl font-medium">
              GET {getModulePingApiPath(MODULE_PREFIX)}
            </h2>
          </div>
          <p>
            The module's apiNamespace middleware stamps{" "}
            <code>{MODULE_API_HEADER_NAME}</code> on every response under{" "}
            <code>{getModuleApiPrefix(MODULE_PREFIX)}</code>.
          </p>
          <Button disabled={isPinging} onClick={handlePing}>
            {isPinging ? "Pinging..." : "Ping module API"}
          </Button>
          {moduleApiHeader && (
            <p data-testid="host-module-api-header">
              {MODULE_API_HEADER_NAME}: {moduleApiHeader}
            </p>
          )}
          {pingError && (
            <Alert variant="error">
              Failed to ping module API: {pingError}
            </Alert>
          )}
        </div>
        <div className="card max-w-2xl space-y-4">
          <div>
            <p className="text-xs font-semibold tracking-wide text-gray-500 uppercase">
              job
            </p>
            <h2 className="text-xl font-medium">
              POST {getModuleJobApiPath(MODULE_PREFIX)}
            </h2>
          </div>
          <p>Submits the module's PgBoss job and returns its ID.</p>
          <Button disabled={isSubmitting} onClick={handleSubmitJob}>
            {isSubmitting ? "Submitting..." : "Submit module job"}
          </Button>
          {jobId && (
            <p data-testid="host-module-job-id">Submitted job: {jobId}</p>
          )}
          {jobError && (
            <Alert variant="error">Failed to submit job: {jobError}</Alert>
          )}
        </div>
      </div>
    </FeatureContainer>
  );
}
