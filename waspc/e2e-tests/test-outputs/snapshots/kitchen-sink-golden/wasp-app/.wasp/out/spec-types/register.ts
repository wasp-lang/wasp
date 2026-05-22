// The import ensures the module is always loaded into the bundle.
// Otherwise, module augmentation can fail if it wasn't loaded.
import "@wasp.sh/spec";

declare module "@wasp.sh/spec" {
  export interface Register {
    entities: {
      User: "User";
      Task: "Task";
      TaskVote: "TaskVote";
      UppercaseTextRequest: "UppercaseTextRequest";
    }
  }
}
