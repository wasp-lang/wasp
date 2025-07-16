import { defineUserSignupFields } from "wasp/server/auth";

export const userSignupFields = defineUserSignupFields({
  username: (data) => {
    if (typeof data.username !== "string") {
      throw new Error("Username is required.");
    }
    if (data.username.length < 6) {
      throw new Error("Username must be at least 6 characters long.");
    }
    return data.username;
  },
});
