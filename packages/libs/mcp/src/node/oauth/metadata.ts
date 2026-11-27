// RFC 9728 (protected resource) and RFC 8414 (authorization server) metadata.
import type { Runtime } from "../runtime";

export function protectedResourceMetadata(rt: Runtime) {
  return {
    resource: rt.urls.mcpResource,
    authorization_servers: [rt.urls.issuer],
    // The starting set (clients request all of it); the rest comes by step-up.
    scopes_supported: rt.initialScopes,
    bearer_methods_supported: ["header"],
    resource_name: `${rt.config.title ?? rt.config.name} MCP server`,
  };
}

export function authorizationServerMetadata(rt: Runtime) {
  return {
    issuer: rt.urls.issuer,
    // The consent page is the authorization endpoint, so it lives on the frontend origin.
    authorization_endpoint: rt.urls.authorizationEndpoint,
    token_endpoint: rt.urls.tokenEndpoint,
    revocation_endpoint: rt.urls.revocationEndpoint,
    response_types_supported: ["code"],
    grant_types_supported: ["authorization_code", "refresh_token"],
    code_challenge_methods_supported: ["S256"],
    token_endpoint_auth_methods_supported: ["none"],
    revocation_endpoint_auth_methods_supported: ["none"],
    scopes_supported: Object.keys(rt.scopes),
    client_id_metadata_document_supported: true,
    authorization_response_iss_parameter_supported: true,
  };
}
