import { expect } from "chai";
import { useMemo } from "react";

import { getSerializedObjects, useQuery } from "wasp/client/operations";

import { FeatureContainer } from "../../../components/FeatureContainer";
import { SERIALIZABLE_OBJECTS_FIXTURE } from "../../../rpcTests/operations/fixtures";

export const SerializationPage = () => {
  const { data: serializedObjects } = useQuery(getSerializedObjects);

  const result = useMemo(() => {
    console.log({ serializedObjects });

    if (!serializedObjects) return;

    try {
      expect(serializedObjects).to.deep.equal(SERIALIZABLE_OBJECTS_FIXTURE);
      return "All serialized objects are of the expected types.";
    } catch (e) {
      console.error(e);
      return String(e);
    }
  }, [serializedObjects]);

  return serializedObjects ? (
    <FeatureContainer>
      <div className="space-y-4">
        <h2 className="feature-title">Serialization</h2>
        <div className="card">
          <label htmlFor="serializedObjects">Serialized objects</label>:
          <p id="serializedObjects">{result}</p>
        </div>
      </div>
    </FeatureContainer>
  ) : null;
};
