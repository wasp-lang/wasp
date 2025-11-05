// ============================================================================
// wasp/auth - Authentication (Available when using auth)
// ============================================================================

declare module "wasp/auth" {
  /**
   * Represents an authenticated user in the Wasp application.
   * This type is available when authentication is enabled in your app.
   */
  export type AuthUser = import("wasp/server/auth").AuthUser;

  /**
   * Gets the email address of the authenticated user.
   * Returns the email if the user has an email auth identity, otherwise undefined.
   *
   * @param user - The authenticated user object
   * @returns The user's email address or undefined
   */
  export function getEmail(user: AuthUser): string | undefined;

  /**
   * Gets the username of the authenticated user.
   * Returns the username if the user has a username auth identity, otherwise undefined.
   *
   * @param user - The authenticated user object
   * @returns The user's username or undefined
   */
  export function getUsername(user: AuthUser): string | undefined;

  /**
   * Gets the first provider user ID for the authenticated user.
   * This is useful when working with OAuth providers or multiple auth methods.
   *
   * @param user - The authenticated user object
   * @returns The provider user ID or undefined
   */
  export function getFirstProviderUserId(user: AuthUser): string | undefined;
}

// ============================================================================
// wasp/client - Client-side configuration and utilities
// ============================================================================

declare module "wasp/client" {
  /**
   * HTTP methods supported by Wasp for API routes and operations.
   */
  export enum HttpMethod {
    Get = "GET",
    Post = "POST",
    Put = "PUT",
    Delete = "DELETE",
  }

  /**
   * Represents a route definition with HTTP method and path.
   */
  export type Route = {
    method: HttpMethod;
    path: string;
  };

  /**
   * Client-side configuration object for the Wasp application.
   * Contains settings like API URL and other client-specific configuration.
   */
  export type ClientConfig = {
    apiUrl: string;
  };

  /**
   * The client configuration object. Use this to access client-side settings.
   */
  export const config: ClientConfig;
}

// ============================================================================
// wasp/client/api - Client-side API utilities
// ============================================================================

declare module "wasp/client/api" {
  import type { AxiosInstance } from "axios";

  /**
   * Pre-configured Axios instance for making API calls to your Wasp backend.
   * Automatically includes authentication headers and proper base URL configuration.
   *
   * @example
   * ```typescript
   * import { api } from 'wasp/client/api';
   *
   * const response = await api.get('/api/users');
   * ```
   */
  export const api: AxiosInstance;
}

// ============================================================================
// wasp/client/auth - Client-side authentication (Available when using auth)
// ============================================================================

declare module "wasp/client/auth" {
  import type { AuthUser } from "wasp/auth";

  /**
   * React hook that provides authentication state and user information.
   * Returns an object containing the current user data and loading state.
   *
   * @returns Object with `data` (AuthUser or null) and loading/error states
   *
   * @example
   * ```typescript
   * function MyComponent() {
   *   const { data: user, isLoading } = useAuth();
   *   if (isLoading) return <div>Loading...</div>;
   *   return <div>Hello {user?.email}</div>;
   * }
   * ```
   */
  export function useAuth(): {
    data: AuthUser | null;
    isLoading: boolean;
    error: Error | null;
  };

  /**
   * Fetches the currently authenticated user.
   * Returns null if no user is authenticated.
   *
   * @returns Promise resolving to the authenticated user or null
   */
  export function getMe(): Promise<AuthUser | null>;

  /**
   * Logs out the current user and clears authentication state.
   *
   * @returns Promise that resolves when logout is complete
   */
  export function logout(): Promise<void>;

  // Email/Username Authentication (Available when using email or username auth)

  /**
   * Signs up a new user with email or username credentials.
   * Available when email or username authentication is configured.
   *
   * @param credentials - User signup credentials
   * @returns Promise that resolves when signup is complete
   */
  export function signup(credentials: {
    email?: string;
    username?: string;
    password: string;
  }): Promise<void>;

  /**
   * Logs in a user with email or username credentials.
   * Available when email or username authentication is configured.
   *
   * @param credentials - User login credentials
   * @returns Promise that resolves when login is complete
   */
  export function login(credentials: {
    email?: string;
    username?: string;
    password: string;
  }): Promise<void>;

  // Email-specific authentication (Available when using email auth)

  /**
   * Requests a password reset email for the given email address.
   * Available when email authentication is configured.
   *
   * @param email - The user's email address
   * @returns Promise that resolves when the reset email is sent
   */
  export function requestPasswordReset(email: string): Promise<void>;

  /**
   * Resets the user's password using a reset token.
   * Available when email authentication is configured.
   *
   * @param data - Password reset data including token and new password
   * @returns Promise that resolves when password is reset
   */
  export function resetPassword(data: {
    token: string;
    password: string;
  }): Promise<void>;

  /**
   * Verifies a user's email address using a verification token.
   * Available when email authentication is configured.
   *
   * @param token - Email verification token
   * @returns Promise that resolves when email is verified
   */
  export function verifyEmail(token: string): Promise<void>;

  // OAuth Sign-in URLs (Available when using respective OAuth providers)

  /**
   * URL for initiating Google OAuth sign-in flow.
   * Available when Google authentication is configured.
   */
  export const googleSignInUrl: string;

  /**
   * URL for initiating GitHub OAuth sign-in flow.
   * Available when GitHub authentication is configured.
   */
  export const githubSignInUrl: string;

  /**
   * URL for initiating Keycloak OAuth sign-in flow.
   * Available when Keycloak authentication is configured.
   */
  export const keycloakSignInUrl: string;

  // Pre-built Auth UI Components

  /**
   * Pre-built login form component with customizable styling and behavior.
   * Available when authentication is configured.
   */
  export const LoginForm: React.ComponentType<{
    appearance?: CustomizationOptions;
  }>;

  /**
   * Pre-built signup form component with customizable styling and behavior.
   * Available when authentication is configured.
   */
  export const SignupForm: React.ComponentType<{
    appearance?: CustomizationOptions;
  }>;

  /**
   * Pre-built forgot password form component.
   * Available when email authentication is configured.
   */
  export const ForgotPasswordForm: React.ComponentType<{
    appearance?: CustomizationOptions;
  }>;

  /**
   * Pre-built email verification form component.
   * Available when email authentication is configured.
   */
  export const VerifyEmailForm: React.ComponentType<{
    appearance?: CustomizationOptions;
  }>;

  /**
   * Pre-built password reset form component.
   * Available when email authentication is configured.
   */
  export const ResetPasswordForm: React.ComponentType<{
    appearance?: CustomizationOptions;
  }>;

  /**
   * Customization options for authentication UI components.
   * Allows styling and behavior customization of pre-built auth forms.
   */
  export type CustomizationOptions = {
    colors?: {
      brand?: string;
      brandAccent?: string;
      submitButtonText?: string;
    };
    appearance?: any;
  };

  // OAuth Sign-in Buttons

  /**
   * Pre-built Google sign-in button component.
   * Available when Google authentication is configured.
   */
  export const GoogleSignInButton: React.ComponentType;

  /**
   * Pre-built Keycloak sign-in button component.
   * Available when Keycloak authentication is configured.
   */
  export const KeycloakSignInButton: React.ComponentType;

  /**
   * Pre-built GitHub sign-in button component.
   * Available when GitHub authentication is configured.
   */
  export const GithubSignInButton: React.ComponentType;
}

// ============================================================================
// wasp/client/crud - Client-side CRUD operations
// ============================================================================

declare module "wasp/client/crud" {
  /**
   * Generated CRUD operations for entities.
   * Replace 'MyCrud' with your actual CRUD name defined in main.wasp.
   * Provides hooks and functions for create, read, update, and delete operations.
   *
   * @example
   * ```typescript
   * import { tasks } from 'wasp/client/crud';
   *
   * function TaskList() {
   *   const { data: tasks } = tasks.getAll.useQuery();
   *   return <div>{tasks?.map(t => t.title)}</div>;
   * }
   * ```
   */
}

// ============================================================================
// wasp/client/operations - Client-side queries and actions
// ============================================================================

declare module "wasp/client/operations" {
  /**
   * React hook for executing Wasp actions on the client.
   * Actions are operations that modify server state.
   *
   * @param actionFn - The action function to execute
   * @returns A tuple with the action executor function and execution state
   *
   * @example
   * ```typescript
   * import { useAction } from 'wasp/client/operations';
   * import { createTask } from 'wasp/client/operations';
   *
   * function CreateTask() {
   *   const createTaskAction = useAction(createTask);
   *   return <button onClick={() => createTaskAction({ title: 'New' })}>
   *     Create
   *   </button>;
   * }
   * ```
   */
  export function useAction<Input, Output>(
    actionFn: (args: Input) => Promise<Output>
  ): [(args: Input) => Promise<Output>, { isLoading: boolean; error: Error | null }];

  /**
   * Configuration for optimistic updates when executing queries.
   * Allows updating UI immediately before server response.
   */
  export type OptimisticUpdateDefinition<Item> = {
    getQuerySpecifier: () => any;
    updateQuery: (oldData: Item[], newItem: Item) => Item[];
  };

  /**
   * React hook for executing Wasp queries on the client.
   * Queries are operations that fetch data from the server.
   *
   * @param queryFn - The query function to execute
   * @param args - Arguments to pass to the query
   * @param options - React Query options
   * @returns Query result with data, loading, and error states
   *
   * @example
   * ```typescript
   * import { useQuery } from 'wasp/client/operations';
   * import { getTasks } from 'wasp/client/operations';
   *
   * function TaskList() {
   *   const { data, isLoading } = useQuery(getTasks);
   *   if (isLoading) return <div>Loading...</div>;
   *   return <div>{data?.map(t => t.title)}</div>;
   * }
   * ```
   */
  export function useQuery<Input, Output>(
    queryFn: (args: Input) => Promise<Output>,
    args?: Input,
    options?: any
  ): {
    data: Output | undefined;
    isLoading: boolean;
    error: Error | null;
    refetch: () => Promise<void>;
  };

  /**
   * Configures the React Query client used by Wasp operations.
   * Allows customization of caching, refetching, and other query behaviors.
   *
   * @param config - React Query client configuration
   *
   * @example
   * ```typescript
   * import { configureQueryClient } from 'wasp/client/operations';
   *
   * configureQueryClient({
   *   defaultOptions: {
   *     queries: { refetchOnWindowFocus: false }
   *   }
   * });
   * ```
   */
  export function configureQueryClient(config: any): void;
}

// ============================================================================
// wasp/client/router - Client-side routing
// ============================================================================

declare module "wasp/client/router" {
  /**
   * Object containing all route definitions in your Wasp app.
   * Each route has a `to` property (path) and a `build` function for generating URLs.
   *
   * @example
   * ```typescript
   * import { routes } from 'wasp/client/router';
   *
   * const taskUrl = routes.TaskRoute.build({ params: { id: '123' } });
   * // Returns: "/tasks/123"
   * ```
   */
  export const routes: Record<
    string,
    {
      to: string;
      build: (options?: {
        params?: Record<string, string | number>;
        search?: Record<string, string>;
        hash?: string;
      }) => string;
    }
  >;

  /**
   * Type-safe Link component for client-side navigation.
   * Wrapper around React Router's Link with Wasp route type safety.
   *
   * @example
   * ```typescript
   * import { Link, routes } from 'wasp/client/router';
   *
   * function Nav() {
   *   return <Link to={routes.TaskRoute.to}>Tasks</Link>;
   * }
   * ```
   */
  export const Link: React.ComponentType<{
    to: string;
    children: React.ReactNode;
    [key: string]: any;
  }>;
}

// ============================================================================
// wasp/client/test - Client-side testing utilities
// ============================================================================

declare module "wasp/client/test" {
  /**
   * Renders a React component within Wasp's context providers for testing.
   * Provides access to React Query, routing, and other Wasp context.
   *
   * @param component - The component to render
   * @returns Rendered component with Wasp context
   *
   * @example
   * ```typescript
   * import { renderInContext } from 'wasp/client/test';
   * import { screen } from '@testing-library/react';
   *
   * test('renders component', () => {
   *   renderInContext(<MyComponent />);
   *   expect(screen.getByText('Hello')).toBeInTheDocument();
   * });
   * ```
   */
  export function renderInContext(
    component: React.ReactElement
  ): ReturnType<typeof import("@testing-library/react").render>;

  /**
   * Mocks the Wasp server for testing purposes.
   * Allows simulating server responses in client-side tests.
   *
   * @param config - Server mock configuration
   *
   * @example
   * ```typescript
   * import { mockServer } from 'wasp/client/test';
   *
   * mockServer({
   *   getTasks: () => [{ id: 1, title: 'Test' }]
   * });
   * ```
   */
  export function mockServer(config: any): void;
}

// ============================================================================
// wasp/client/webSocket - Client-side WebSocket support
// ============================================================================

declare module "wasp/client/webSocket" {
  import type { Socket } from "socket.io-client";

  /**
   * Extracts the payload type for server-to-client WebSocket events.
   *
   * @template Event - The event name
   */
  export type ServerToClientPayload<Event extends string> = any;

  /**
   * Extracts the payload type for client-to-server WebSocket events.
   *
   * @template Event - The event name
   */
  export type ClientToServerPayload<Event extends string> = any;

  /**
   * React hook that provides access to the WebSocket connection.
   * Returns the Socket.IO client instance.
   *
   * @returns Object containing the socket instance
   *
   * @example
   * ```typescript
   * import { useSocket } from 'wasp/client/webSocket';
   *
   * function Chat() {
   *   const { socket } = useSocket();
   *   const sendMessage = () => socket.emit('message', 'Hello!');
   *   return <button onClick={sendMessage}>Send</button>;
   * }
   * ```
   */
  export function useSocket(): { socket: Socket };

  /**
   * React hook for listening to WebSocket events from the server.
   * Automatically handles cleanup when component unmounts.
   *
   * @param event - The event name to listen for
   * @param handler - Callback function to handle the event
   *
   * @example
   * ```typescript
   * import { useSocketListener } from 'wasp/client/webSocket';
   *
   * function Chat() {
   *   useSocketListener('newMessage', (msg) => {
   *     console.log('Received:', msg);
   *   });
   *   return <div>Chat</div>;
   * }
   * ```
   */
  export function useSocketListener<Event extends string>(
    event: Event,
    handler: (payload: any) => void
  ): void;
}

// ============================================================================
// wasp/dev - Development utilities (for Tailwind config, etc.)
// ============================================================================

declare module "wasp/dev" {
  /**
   * Resolves a project path relative to the Wasp project root.
   * Useful in development tools like Tailwind config.
   *
   * @param path - Path relative to project root
   * @returns Resolved absolute path
   *
   * @example
   * ```typescript
   * import { resolveProjectPath } from 'wasp/dev';
   *
   * const srcPath = resolveProjectPath('src/styles');
   * ```
   */
  export function resolveProjectPath(path: string): string;
}

// ============================================================================
// wasp/entities - Database entity types
// ============================================================================

declare module "wasp/entities" {
  /**
   * Database entity types generated from your Prisma schema.
   * Replace 'MyEntity' with your actual entity names (e.g., User, Task, etc.).
   *
   * These types are automatically generated based on your schema.prisma file.
   *
   * @example
   * ```typescript
   * import type { User, Task } from 'wasp/entities';
   *
   * function processUser(user: User) {
   *   console.log(user.email);
   * }
   * ```
   */

  /**
   * Auth entity type representing authentication records.
   * Available when authentication is enabled.
   */
  export type Auth = {
    id: string;
    userId?: string;
  };

  /**
   * AuthIdentity entity type representing authentication provider identities.
   * Available when authentication is enabled.
   */
  export type AuthIdentity = {
    providerName: string;
    providerUserId: string;
    providerData: string;
    authId: string;
  };
}

// ============================================================================
// wasp/server - Server-side utilities and configuration
// ============================================================================

declare module "wasp/server" {
  import type { PrismaClient as PC } from "@prisma/client";

  /**
   * Prisma client instance for database operations.
   * Provides full access to your database schema.
   *
   * @example
   * ```typescript
   * import { prisma } from 'wasp/server';
   *
   * export const getUsers = async () => {
   *   return prisma.user.findMany();
   * };
   * ```
   */
  export const prisma: PC;

  /**
   * Prisma client type, useful for type annotations.
   */
  export type PrismaClient = PC;

  /**
   * Server-side configuration object.
   * Contains server-specific settings and environment variables.
   */
  export const config: {
    frontendUrl: string;
    databaseUrl: string;
  };

  /**
   * Function type for server setup operations.
   * Used to configure Express app and HTTP server before starting.
   *
   * @param context - Server setup context with app and server instances
   *
   * @example
   * ```typescript
   * import { ServerSetupFn } from 'wasp/server';
   *
   * export const serverSetup: ServerSetupFn = async ({ app }) => {
   *   app.use(express.json());
   * };
   * ```
   */
  export type ServerSetupFn = (context: {
    app: any;
    server: any;
  }) => Promise<void>;

  /**
   * HTTP error class for throwing errors with specific status codes.
   * Can include additional data in the error response.
   *
   * @example
   * ```typescript
   * import { HttpError } from 'wasp/server';
   *
   * export const getTask = async (args, context) => {
   *   if (!context.user) {
   *     throw new HttpError(401, 'Unauthorized');
   *   }
   *   throw new HttpError(404, 'Not found', { taskId: args.id });
   * };
   * ```
   */
  export class HttpError extends Error {
    constructor(statusCode: number, message: string, data?: any);
    statusCode: number;
    data?: any;
  }

  /**
   * Function type for configuring Express middleware.
   * Used to add custom middleware to your Wasp server.
   *
   * @param middlewareConfig - Express middleware configuration
   *
   * @example
   * ```typescript
   * import { MiddlewareConfigFn } from 'wasp/server';
   * import cors from 'cors';
   *
   * export const middleware: MiddlewareConfigFn = (config) => {
   *   return config.use(cors());
   * };
   * ```
   */
  export type MiddlewareConfigFn = (middlewareConfig: any) => any;

  /**
   * Function type for database seeding operations.
   * Used to populate your database with initial data.
   *
   * @param prisma - Prisma client instance
   *
   * @example
   * ```typescript
   * import { DbSeedFn } from 'wasp/server';
   *
   * export const seedDatabase: DbSeedFn = async (prisma) => {
   *   await prisma.user.create({
   *     data: { email: 'test@example.com' }
   *   });
   * };
   * ```
   */
  export type DbSeedFn = (prisma: PrismaClient) => Promise<void>;
}

// ============================================================================
// wasp/server/api - Server-side API routes
// ============================================================================

declare module "wasp/server/api" {
  import type { Request, Response, NextFunction } from "express";

  /**
   * Type for custom API route handlers.
   * Replace 'MyApiRoute' with your actual API route name.
   *
   * API routes provide full Express request/response control.
   *
   * @example
   * ```typescript
   * // In your API route file
   * export const myApiRoute = (req, res) => {
   *   res.json({ message: 'Hello!' });
   * };
   * ```
   */
}

// ============================================================================
// wasp/server/auth - Server-side authentication utilities
// ============================================================================

declare module "wasp/server/auth" {
  import type { AuthUser } from "wasp/auth";
  import type { PrismaClient } from "wasp/server";

  /**
   * Defines custom signup fields for user registration.
   * Allows adding extra fields to the signup process.
   *
   * @param fields - Field definitions with validation
   * @returns Configured signup fields
   *
   * @example
   * ```typescript
   * import { defineUserSignupFields } from 'wasp/server/auth';
   *
   * export const signupFields = defineUserSignupFields({
   *   displayName: (data) => data.displayName,
   *   age: (data) => {
   *     if (data.age < 18) throw new Error('Must be 18+');
   *     return data.age;
   *   }
   * });
   * ```
   */
  export function defineUserSignupFields(fields: any): any;

  /**
   * Creates a provider ID for authentication identity lookups.
   *
   * @param providerName - Name of the auth provider (e.g., 'email', 'google')
   * @param providerUserId - User ID from the provider
   * @returns Composite provider ID
   */
  export function createProviderId(
    providerName: string,
    providerUserId: string
  ): ProviderId;

  /**
   * Sanitizes and serializes provider data for storage.
   *
   * @param providerData - Raw provider data
   * @returns Serialized provider data string
   */
  export function sanitizeAndSerializeProviderData(providerData: any): string;

  /**
   * Updates authentication identity provider data in the database.
   *
   * @param providerId - The provider ID
   * @param providerData - New provider data
   * @param prisma - Prisma client instance
   * @returns Updated auth identity
   */
  export function updateAuthIdentityProviderData(
    providerId: ProviderId,
    providerData: any,
    prisma: PrismaClient
  ): Promise<any>;

  /**
   * Retrieves provider data for an authentication identity.
   *
   * @param providerId - The provider ID
   * @param prisma - Prisma client instance
   * @returns Provider data
   */
  export function getProviderData(
    providerId: ProviderId,
    prisma: PrismaClient
  ): Promise<EmailProviderData | UsernameProviderData | OAuthProviderData>;

  /**
   * Retrieves provider data including password hash.
   * Used for password-based authentication.
   *
   * @param providerId - The provider ID
   * @param prisma - Prisma client instance
   * @returns Provider data with password
   */
  export function getProviderDataWithPassword(
    providerId: ProviderId,
    prisma: PrismaClient
  ): Promise<any>;

  /**
   * Finds an authentication identity by provider ID.
   *
   * @param providerId - The provider ID
   * @param prisma - Prisma client instance
   * @returns Auth identity or null
   */
  export function findAuthIdentity(
    providerId: ProviderId,
    prisma: PrismaClient
  ): Promise<any | null>;

  /**
   * Creates a new user with authentication identity.
   *
   * @param providerId - The provider ID
   * @param providerData - Provider-specific data
   * @param userData - Additional user data
   * @param prisma - Prisma client instance
   * @returns Created user result
   */
  export function createUser(
    providerId: ProviderId,
    providerData: any,
    userData: any,
    prisma: PrismaClient
  ): Promise<CreateUserResult>;

  /**
   * Result type for user creation operations.
   */
  export type CreateUserResult = {
    user: AuthUser;
    identities: any[];
  };

  /**
   * Composite provider ID type.
   */
  export type ProviderId = {
    providerName: ProviderName;
    providerUserId: string;
  };

  /**
   * Authentication provider name type.
   */
  export type ProviderName =
    | "email"
    | "username"
    | "google"
    | "github"
    | "keycloak"
    | "discord"
    | string;

  /**
   * Email provider data type.
   */
  export type EmailProviderData = {
    isEmailVerified: boolean;
    emailVerificationSentAt: string | null;
    passwordResetSentAt: string | null;
  };

  /**
   * Username provider data type.
   */
  export type UsernameProviderData = Record<string, never>;

  /**
   * OAuth provider data type.
   */
  export type OAuthProviderData = Record<string, any>;

  /**
   * Ensures that a password is present in the data.
   * Throws an error if password is missing.
   *
   * @param data - Data object to check
   */
  export function ensurePasswordIsPresent(data: any): void;

  /**
   * Validates password strength and format.
   * Throws an error if password is invalid.
   *
   * @param password - Password to validate
   */
  export function ensureValidPassword(password: string): void;

  // Email-specific authentication utilities

  /**
   * Function type for generating verification email content.
   *
   * @param params - Email generation parameters
   * @returns Email subject and content
   *
   * @example
   * ```typescript
   * export const getVerificationEmail: GetVerificationEmailContentFn =
   *   ({ verificationLink }) => ({
   *     subject: 'Verify your email',
   *     text: `Click here: ${verificationLink}`,
   *     html: `<a href="${verificationLink}">Verify</a>`
   *   });
   * ```
   */
  export type GetVerificationEmailContentFn = (params: {
    verificationLink: string;
  }) => {
    subject: string;
    text: string;
    html: string;
  };

  /**
   * Function type for generating password reset email content.
   *
   * @param params - Email generation parameters
   * @returns Email subject and content
   */
  export type GetPasswordResetEmailContentFn = (params: {
    passwordResetLink: string;
  }) => {
    subject: string;
    text: string;
    html: string;
  };

  /**
   * Creates an email verification link for a user.
   *
   * @param email - User's email address
   * @returns Verification link
   */
  export function createEmailVerificationLink(email: string): Promise<string>;

  /**
   * Sends an email verification email to a user.
   *
   * @param email - User's email address
   * @param verificationLink - Verification link to include
   */
  export function sendEmailVerificationEmail(
    email: string,
    verificationLink: string
  ): Promise<void>;

  /**
   * Creates a password reset link for a user.
   *
   * @param email - User's email address
   * @returns Password reset link
   */
  export function createPasswordResetLink(email: string): Promise<string>;

  /**
   * Sends a password reset email to a user.
   *
   * @param email - User's email address
   * @param passwordResetLink - Reset link to include
   */
  export function sendPasswordResetEmail(
    email: string,
    passwordResetLink: string
  ): Promise<void>;

  /**
   * Checks if an email resend is allowed based on rate limiting.
   *
   * @param user - The user to check
   * @returns Whether resend is allowed
   */
  export function isEmailResendAllowed(user: AuthUser): boolean;

  /**
   * Validates email format.
   * Throws an error if email is invalid.
   *
   * @param email - Email to validate
   */
  export function ensureValidEmail(email: string): void;

  /**
   * Validates username format.
   * Throws an error if username is invalid.
   *
   * @param username - Username to validate
   */
  export function ensureValidUsername(username: string): void;

  // Authentication hooks

  /**
   * Hook function called before user signup.
   * Can be used to validate or modify signup data.
   */
  export type OnBeforeSignupHook = (params: {
    providerId: ProviderId;
    prisma: PrismaClient;
  }) => Promise<void>;

  /**
   * Hook function called after user signup.
   * Can be used for post-signup operations like sending welcome emails.
   */
  export type OnAfterSignupHook = (params: {
    providerId: ProviderId;
    user: AuthUser;
    prisma: PrismaClient;
  }) => Promise<void>;

  /**
   * Hook function called after email verification.
   * Available when email authentication is configured.
   */
  export type OnAfterEmailVerified = (params: {
    user: AuthUser;
    prisma: PrismaClient;
  }) => Promise<void>;

  /**
   * Hook function called before OAuth redirect.
   * Can be used to modify OAuth flow.
   */
  export type OnBeforeOAuthRedirectHook = (params: {
    url: URL;
  }) => Promise<{ url: URL }>;

  /**
   * Hook function called before user login.
   * Can be used to validate login attempts.
   */
  export type OnBeforeLoginHook = (params: {
    providerId: ProviderId;
    prisma: PrismaClient;
  }) => Promise<void>;

  /**
   * Hook function called after user login.
   * Can be used for post-login operations like logging.
   */
  export type OnAfterLoginHook = (params: {
    providerId: ProviderId;
    user: AuthUser;
    prisma: PrismaClient;
  }) => Promise<void>;
}

// ============================================================================
// wasp/auth/providers - Authentication provider types
// ============================================================================

declare module "wasp/auth/providers" {
  /**
   * Type for username and password signup fields.
   * Derived from defineUserSignupFields when using username auth.
   */
  export type UserUsernameAndPasswordSignupFields = any;

  /**
   * Type for email signup fields.
   * Derived from defineUserSignupFields when using email auth.
   */
  export type UserEmailSignupFields = any;
}

// ============================================================================
// wasp/server/crud - Server-side CRUD operations
// ============================================================================

declare module "wasp/server/crud" {
  /**
   * Generated CRUD operation types for entities.
   * Replace 'MyCrud' with your actual CRUD name defined in main.wasp.
   *
   * Provides typed CRUD operations that can be customized on the server.
   */
}

// ============================================================================
// wasp/server/email - Server-side email utilities
// ============================================================================

declare module "wasp/server/email" {
  /**
   * Pre-configured email sender instance.
   * Use this to send emails from your Wasp server.
   *
   * @example
   * ```typescript
   * import { emailSender } from 'wasp/server/email';
   *
   * export const sendWelcomeEmail = async (user) => {
   *   await emailSender.send({
   *     to: user.email,
   *     subject: 'Welcome!',
   *     text: 'Thanks for signing up!',
   *     html: '<p>Thanks for signing up!</p>'
   *   });
   * };
   * ```
   */
  export const emailSender: EmailSender;

  /**
   * Email message type for sending emails.
   */
  export type Email = {
    from?: EmailFromField;
    to: string;
    subject: string;
    text: string;
    html: string;
  };

  /**
   * Email sender field type.
   */
  export type EmailFromField = {
    name?: string;
    email: string;
  };

  /**
   * Email sender interface.
   */
  export type EmailSender = {
    send(email: Email): Promise<SentMessageInfo>;
  };

  /**
   * Information about a sent email message.
   */
  export type SentMessageInfo = {
    accepted: string[];
    rejected: string[];
    messageId: string;
  };
}

// ============================================================================
// wasp/server/jobs - Server-side background jobs
// ============================================================================

declare module "wasp/server/jobs" {
  /**
   * Your defined background job types.
   * Replace 'MyJob' with your actual job name defined in main.wasp.
   *
   * Jobs allow you to execute tasks asynchronously in the background.
   *
   * @example
   * ```typescript
   * // In main.wasp:
   * // job myJob {
   * //   executor: PgBoss,
   * //   perform: { fn: import { myJob } from "@src/jobs" }
   * // }
   *
   * // In your code:
   * import { myJob } from 'wasp/server/jobs';
   *
   * export const scheduleJob = async () => {
   *   await myJob.submit({ data: 'example' });
   * };
   * ```
   */
}

// ============================================================================
// wasp/server/operations - Server-side operations (queries and actions)
// ============================================================================

declare module "wasp/server/operations" {
  import type { AuthUser } from "wasp/auth";
  import type { PrismaClient } from "wasp/server";

  /**
   * Your defined query types.
   * Replace 'MyQuery' with your actual query name.
   *
   * Queries are read operations that fetch data.
   *
   * @example
   * ```typescript
   * // Query type is automatically generated
   * export const getTasks: MyQuery<void, Task[]> =
   *   async (args, context) => {
   *     return context.entities.Task.findMany();
   *   };
   * ```
   */

  /**
   * Your defined action types.
   * Replace 'MyAction' with your actual action name.
   *
   * Actions are write operations that modify data.
   *
   * @example
   * ```typescript
   * // Action type is automatically generated
   * export const createTask: MyAction<{ title: string }, Task> =
   *   async (args, context) => {
   *     return context.entities.Task.create({
   *       data: { title: args.title }
   *     });
   *   };
   * ```
   */
}

// ============================================================================
// wasp/server/webSocket - Server-side WebSocket support
// ============================================================================

declare module "wasp/server/webSocket" {
  import type { Server } from "socket.io";
  import type { AuthUser } from "wasp/auth";
  import type { PrismaClient } from "wasp/server";

  /**
   * WebSocket definition function type.
   * Used to configure Socket.IO server with custom events.
   *
   * @param io - Socket.IO server instance
   * @param context - Server context with database access
   *
   * @example
   * ```typescript
   * import { WebSocketDefinition } from 'wasp/server/webSocket';
   *
   * export const webSocketFn: WebSocketDefinition = (io, context) => {
   *   io.on('connection', (socket) => {
   *     socket.on('message', async (msg) => {
   *       const user = socket.data.user;
   *       io.emit('message', { user: user?.id, msg });
   *     });
   *   });
   * };
   * ```
   */
  export type WebSocketDefinition<
    ClientToServerEvents = any,
    ServerToClientEvents = any,
    InterServerEvents = any,
    SocketData extends WaspSocketData = WaspSocketData
  > = (
    io: Server<
      ClientToServerEvents,
      ServerToClientEvents,
      InterServerEvents,
      SocketData
    >,
    context: {
      entities: Record<string, any>;
    }
  ) => Promise<void> | void;

  /**
   * Socket data interface containing user information.
   * Attached to each WebSocket connection.
   */
  export interface WaspSocketData {
    user?: AuthUser;
  }
}

// ============================================================================
// wasp/client/env - Client environment variables
// ============================================================================

declare module "wasp/client/env" {
  /**
   * Client-side environment variables.
   * Only includes variables prefixed with REACT_APP_.
   *
   * @example
   * ```typescript
   * import { env } from 'wasp/client/env';
   *
   * const apiKey = env.REACT_APP_API_KEY;
   * ```
   */
  export const env: Record<string, string>;
}

// ============================================================================
// wasp/server/env - Server environment variables
// ============================================================================

declare module "wasp/server/env" {
  /**
   * Server-side environment variables.
   * Includes all environment variables available to the server.
   *
   * @example
   * ```typescript
   * import { env } from 'wasp/server/env';
   *
   * const databaseUrl = env.DATABASE_URL;
   * ```
   */
  export const env: Record<string, string>;
}

// ============================================================================
// wasp/env - Environment variable validation
// ============================================================================

declare module "wasp/env" {
  /**
   * Defines a validation schema for environment variables.
   * Used to ensure required env vars are present and valid.
   *
   * @param schema - Zod-like validation schema
   * @returns Validated environment object
   *
   * @example
   * ```typescript
   * import { defineEnvValidationSchema } from 'wasp/env';
   * import { z } from 'zod';
   *
   * export const env = defineEnvValidationSchema({
   *   server: z.object({
   *     API_KEY: z.string(),
   *     PORT: z.string().regex(/^\d+$/)
   *   }),
   *   client: z.object({
   *     REACT_APP_API_URL: z.string().url()
   *   })
   * });
   * ```
   */
  export function defineEnvValidationSchema(schema: any): any;
}
