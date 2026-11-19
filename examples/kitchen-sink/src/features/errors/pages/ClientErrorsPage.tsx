import { Component, type ReactNode, useState } from "react";
import { Button } from "../../../components/Button";
import { FeatureContainer } from "../../../components/FeatureContainer";

export const ClientErrorsPage = () => {
  const [shouldThrowWhileRendering, setShouldThrowWhileRendering] =
    useState(false);
  const [shouldThrowInsideErrorBoundary, setShouldThrowInsideErrorBoundary] =
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
        <Button onClick={() => setShouldThrowInsideErrorBoundary(true)}>
          Throw inside an error boundary
        </Button>
        <WidgetErrorBoundary>
          {shouldThrowInsideErrorBoundary && <ThrowWhileRendering />}
        </WidgetErrorBoundary>
      </div>
    </FeatureContainer>
  );
};

function ThrowWhileRendering(): never {
  throw new Error("Thrown while rendering");
}

class WidgetErrorBoundary extends Component<
  { children: ReactNode },
  { hasError: boolean }
> {
  override state = { hasError: false };

  static getDerivedStateFromError(): { hasError: boolean } {
    return { hasError: true };
  }

  override render(): ReactNode {
    return this.state.hasError ? (
      <p>The widget failed.</p>
    ) : (
      this.props.children
    );
  }
}
