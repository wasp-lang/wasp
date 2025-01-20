export declare const env: {
    PORT: number;
    DATABASE_URL: string;
    SENDGRID_API_KEY: string;
    SKIP_EMAIL_VERIFICATION_IN_DEV: boolean;
    GOOGLE_CLIENT_ID: string;
    GOOGLE_CLIENT_SECRET: string;
    NODE_ENV: "development";
    WASP_SERVER_URL: string;
    WASP_WEB_CLIENT_URL: string;
    JWT_SECRET: string;
    PG_BOSS_NEW_OPTIONS?: string | undefined;
} | {
    PORT: number;
    DATABASE_URL: string;
    SENDGRID_API_KEY: string;
    SKIP_EMAIL_VERIFICATION_IN_DEV: boolean;
    GOOGLE_CLIENT_ID: string;
    GOOGLE_CLIENT_SECRET: string;
    NODE_ENV: "production";
    WASP_SERVER_URL: string;
    WASP_WEB_CLIENT_URL: string;
    JWT_SECRET: string;
    PG_BOSS_NEW_OPTIONS?: string | undefined;
};
