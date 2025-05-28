import { expect } from "chai";
import { useMemo } from "react";
import { getSerializedObjects, useQuery } from "wasp/client/operations";
import { SimplePageContainer } from "../../components/SimplePageContainer";
import { SERIALIZABLE_OBJECTS_FIXTURE } from "./fixtures";

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
    <SimplePageContainer>
      <div className="card">
        <label htmlFor="serializedObjects">Serialized objects</label>:
        <p id="serializedObjects">{result}</p>
      </div>
    </SimplePageContainer>
  ) : null;
};
