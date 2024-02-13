// PUBLIC API
// NOTE: This is enough to cover Operations and our APIs (src/Wasp/AppSpec/Api.hs).
export enum HttpMethod {
	Get = 'GET',
	Post = 'POST',
	Put = 'PUT',
	Delete = 'DELETE',
}

// PUBLIC API
export type Route = { method: HttpMethod; path: string }
