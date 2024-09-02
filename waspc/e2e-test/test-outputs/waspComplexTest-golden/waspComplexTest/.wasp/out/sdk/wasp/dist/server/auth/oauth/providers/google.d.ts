import { Google } from "arctic";
export declare const google: {
    id: string;
    displayName: string;
    env: Record<"GOOGLE_CLIENT_ID" | "GOOGLE_CLIENT_SECRET", string>;
    oAuthClient: Google;
};
