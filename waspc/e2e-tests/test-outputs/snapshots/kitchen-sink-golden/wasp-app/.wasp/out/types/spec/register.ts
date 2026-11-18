// The import ensures the module is always loaded into the bundle.
// Otherwise, module augmentation can fail if it wasn't loaded.
import "@wasp/spec";

declare module "@wasp/spec" {
  export interface Register {
    entities: {
      User: "User";
      Task: "Task";
      TaskVote: "TaskVote";
      UppercaseTextRequest: "UppercaseTextRequest";
    }
  }
}
