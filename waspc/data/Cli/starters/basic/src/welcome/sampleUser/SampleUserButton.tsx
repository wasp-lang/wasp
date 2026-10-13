import { useState } from "react";
import { api } from "wasp/client/api";
import { Link } from "wasp/client/router";
import { Button } from "../../shared/components/Button";
import { InlineCode } from "../components/InlineCode";
import type { SampleUser } from "./apis";

// Development-only, see `./apis.ts` for how to remove it.
export function SampleUserButton() {
  const [sampleUser, setSampleUser] = useState<SampleUser | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  async function createSampleUser() {
    setIsLoading(true);
    setError(null);
    try {
      setSampleUser(await api.post("dev/sample-user").json<SampleUser>());
    } catch (err: unknown) {
      setError(err instanceof Error ? err.message : "Something went wrong.");
    } finally {
      setIsLoading(false);
    }
  }

  return (
    <>
      <Button
        size="sm"
        variant="ghost"
        className="border border-neutral-900 disabled:cursor-not-allowed disabled:opacity-40"
        onClick={createSampleUser}
        disabled={isLoading}
      >
        Create a sample user
      </Button>
      {sampleUser && (
        <p className="w-full">
          <Link to="/login" className="font-semibold underline">
            Log in
          </Link>{" "}
          with <InlineCode>{sampleUser.email}</InlineCode> and password{" "}
          <InlineCode>{sampleUser.password}</InlineCode>.
        </p>
      )}
      {error && <p className="w-full text-red-600">{error}</p>}
    </>
  );
}
