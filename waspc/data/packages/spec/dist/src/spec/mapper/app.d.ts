import * as AppSpec from "../../appSpec.js";
import * as WaspSpec from "../publicApi/waspSpec.js";
import { AppMapperContext } from "./context.js";
export declare function mapAppSpec(app: WaspSpec.App, ctx: AppMapperContext): AppSpec.GetDeclForType<"App">;
export declare function mapDeployment(deployment: WaspSpec.Deployment): AppSpec.Deployment;
export declare function mapAuth(auth: WaspSpec.Auth, ctx: AppMapperContext): AppSpec.Auth;
export declare function mapAuthMethods(methods: WaspSpec.AuthMethods, ctx: AppMapperContext): AppSpec.AuthMethods;
export declare function mapUsernameAndPassword(usernameAndPassword: WaspSpec.UsernameAndPasswordConfig, ctx: AppMapperContext): AppSpec.UsernameAndPasswordConfig;
export declare function mapSocialAuth(socialAuth: WaspSpec.SocialAuthConfig, ctx: AppMapperContext): AppSpec.ExternalAuthConfig;
export declare function mapEmailAuth(emailAuth: WaspSpec.EmailAuthConfig, ctx: AppMapperContext): AppSpec.EmailAuthConfig;
export declare function mapEmailFlow(emailFlow: WaspSpec.EmailFlowConfig, ctx: AppMapperContext): AppSpec.EmailVerificationConfig;
export declare function mapServer(server: WaspSpec.Server, ctx: AppMapperContext): AppSpec.Server;
export declare function mapClient(client: WaspSpec.Client, ctx: AppMapperContext): AppSpec.Client;
export declare function mapDb(db: WaspSpec.Db, ctx: AppMapperContext): AppSpec.Db;
export declare function mapEmailSender(emailSender: WaspSpec.EmailSender): AppSpec.EmailSender;
export declare function mapEmailFromField(emailFromField: WaspSpec.EmailFromField): AppSpec.EmailFromField;
export declare function mapWebSocket(webSocket: WaspSpec.WebSocket, ctx: AppMapperContext): AppSpec.WebSocket;
//# sourceMappingURL=app.d.ts.map