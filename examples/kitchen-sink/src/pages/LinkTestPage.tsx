import { useState } from "react";
import { Link } from "wasp/client/router";

import { Button } from "../components/Button";
import { FeatureContainer } from "../components/FeatureContainer";

export function LinkTestPage() {
  const [searchValue, setSearchValue] = useState<"foo" | "bar">("foo");
  const [hash, setHash] = useState("top");

  return (
    <FeatureContainer>
      <div className="space-y-6">
        <h2 className="feature-title">Link Component</h2>

        <div className="card space-y-4">
          <h3 className="font-semibold">Search Prop Test</h3>
          <p className="text-sm text-gray-600">
            Tests that Link href updates when search prop changes.
          </p>
          <div className="flex items-center gap-4">
            <Link to="/login" search={{ q: searchValue }} id="searchLink">
              Login Link
            </Link>
            <span id="toggleSearch">
              <Button
                onClick={() =>
                  setSearchValue(searchValue === "foo" ? "bar" : "foo")
                }
              >
                Toggle Search
              </Button>
            </span>
          </div>
          <p className="text-sm" id="searchStatus">
            Current: q={searchValue}
          </p>
        </div>

        <div className="card space-y-4">
          <h3 className="font-semibold">Hash Prop Test</h3>
          <p className="text-sm text-gray-600">
            Tests that Link href updates when hash prop changes.
          </p>
          <div className="flex items-center gap-4">
            <Link to="/" hash={hash} id="hashLink">
              Home Link
            </Link>
            <span id="toggleHash">
              <Button
                onClick={() => setHash(hash === "top" ? "bottom" : "top")}
              >
                Toggle Hash
              </Button>
            </span>
          </div>
          <p className="text-sm" id="hashStatus">
            Current: #{hash}
          </p>
        </div>
      </div>
    </FeatureContainer>
  );
}
