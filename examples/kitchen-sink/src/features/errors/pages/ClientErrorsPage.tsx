import { useState } from "react";
import { Button } from "../../../components/Button";
import { FeatureContainer } from "../../../components/FeatureContainer";

export const ClientErrorsPage = () => {
  const [shouldThrowWhileRendering, setShouldThrowWhileRendering] =
    useState(false);

  return (
    <FeatureContainer>
      <div className="space-y-4">
        <h2 className="feature-title">Client errors</h2>
        <Button
          onClick={() => {
            throw new Error("Thrown in an event handler");
          }}
        >
          Throw in an event handler
        </Button>
        <Button
          onClick={() => {
            void Promise.reject(new Error("Rejected without a catch"));
          }}
        >
          Reject a promise
        </Button>
        <Button onClick={() => setShouldThrowWhileRendering(true)}>
          Throw while rendering
        </Button>
        {shouldThrowWhileRendering && <ThrowWhileRendering />}
      </div>
    </FeatureContainer>
  );
};

function ThrowWhileRendering(): never {
  throw new Error("Thrown while rendering");
}
