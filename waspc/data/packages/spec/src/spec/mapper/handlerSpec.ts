import { isEqual } from "es-toolkit";
import { isRefObjectLike, type RefObject } from "../refObject.js";
import { WaspSpecUserError } from "../waspSpecUserError.js";

/**
 * A handler's `spec` is one object mixing plain data with references to
 * app code. A reference cannot cross the compiler as data, so it is lifted
 * out here, keyed by where it sat; the generated code imports each one and
 * sets it back at its path before calling the handler's adapter.
 */
export type SplitHandlerSpec = {
  /** The spec with every reference removed, as JSON. Undefined when there is no spec. */
  dataJson: string | undefined;
  /**
   * The lifted references. A key is the JSON-encoded array of path segments
   * (`["methods","google","configFn"]`), so a spec key containing a '.'
   * cannot be misread as nesting.
   */
  references: Record<string, RefObject>;
};

export function splitHandlerSpec(
  schemeName: string,
  side: "server" | "client",
  spec: unknown,
): SplitHandlerSpec {
  if (spec === undefined) {
    return { dataJson: undefined, references: {} };
  }
  const where = `Auth scheme '${schemeName}' has a ${side} spec`;
  // A reference is a plain object too, but a spec that IS one has no
  // fields for the handler to read.
  if (!isPlainObject(spec) || isRefObjectLike(spec)) {
    throw new WaspSpecUserError(`${where} that is not a plain object.`);
  }

  const references: Record<string, RefObject> = {};

  const withoutReferences = (value: unknown, path: string[]): unknown => {
    if (isRefObjectLike(value)) {
      references[JSON.stringify(path)] = value;
      return undefined;
    }
    if (Array.isArray(value)) {
      // An array has no stable keys to set a reference back at.
      if (containsReference(value)) {
        throw new WaspSpecUserError(
          `${where} with a reference inside an array at '${path.join(".")}'. Put references in object fields instead.`,
        );
      }
      return value;
    }
    if (isPlainObject(value)) {
      const data: Record<string, unknown> = {};
      for (const [key, child] of Object.entries(value)) {
        const childData = withoutReferences(child, [...path, key]);
        if (!isRefObjectLike(child)) {
          data[key] = childData;
        }
      }
      return data;
    }
    return value;
  };

  const data = withoutReferences(spec, []);

  // The data travels to the generated code as JSON, so anything that doesn't
  // survive the round-trip (functions, class instances, undefined-holed
  // arrays) would arrive silently mangled. Rejecting here turns that into a
  // compile error; app code belongs in the spec as a REFERENCE.
  const dataJson = JSON.stringify(data);
  if (dataJson === undefined || !isEqual(JSON.parse(dataJson), data)) {
    throw new WaspSpecUserError(
      `${where} that does not survive JSON serialization. Apart from references to your code, a handler's spec must be plain serializable data.`,
    );
  }
  return { dataJson, references };
}

function containsReference(value: unknown): boolean {
  if (isRefObjectLike(value)) return true;
  if (Array.isArray(value)) return value.some(containsReference);
  if (isPlainObject(value)) return Object.values(value).some(containsReference);
  return false;
}

function isPlainObject(value: unknown): value is Record<string, unknown> {
  return (
    typeof value === "object" &&
    value !== null &&
    !Array.isArray(value) &&
    Object.getPrototypeOf(value) === Object.prototype
  );
}
