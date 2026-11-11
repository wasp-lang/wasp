import { config } from 'wasp/server'

const openApiDocument = {
  "components": {
    "securitySchemes": {
      "bearerAuth": {
        "description": "The session ID of a logged-in user.",
        "scheme": "bearer",
        "type": "http"
      }
    }
  },
  "info": {
    "title": "Wasp Kitchen Sink",
    "version": "0.0.0"
  },
  "openapi": "3.1.0",
  "paths": {
    "/api/streaming-test": {
      "get": {
        "operationId": "streamingText",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": [
          {
            "bearerAuth": []
          },
          {}
        ]
      }
    },
    "/bar/baz": {
      "get": {
        "operationId": "barBaz",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": []
      },
      "head": {
        "operationId": "headBarBaz",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": []
      },
      "patch": {
        "operationId": "patchBarBaz",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": []
      }
    },
    "/errors/concealed": {
      "get": {
        "operationId": "throwConcealedError",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": []
      }
    },
    "/errors/http": {
      "get": {
        "operationId": "throwHttpError",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": []
      }
    },
    "/errors/rate-limit": {
      "get": {
        "operationId": "throwRateLimitError",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": []
      }
    },
    "/errors/unavailable": {
      "get": {
        "operationId": "throwUnavailableError",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": []
      }
    },
    "/errors/unexpected": {
      "get": {
        "operationId": "throwUnexpectedError",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": []
      }
    },
    "/errors/upstream": {
      "get": {
        "operationId": "throwUpstreamError",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": []
      }
    },
    "/foo/bar": {
      "delete": {
        "operationId": "fooBar_delete",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": [
          {
            "bearerAuth": []
          },
          {}
        ]
      },
      "get": {
        "operationId": "fooBar_get",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": [
          {
            "bearerAuth": []
          },
          {}
        ]
      },
      "head": {
        "operationId": "fooBar_head",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": [
          {
            "bearerAuth": []
          },
          {}
        ]
      },
      "patch": {
        "operationId": "fooBar_patch",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": [
          {
            "bearerAuth": []
          },
          {}
        ]
      },
      "post": {
        "operationId": "fooBar_post",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": [
          {
            "bearerAuth": []
          },
          {}
        ]
      },
      "put": {
        "operationId": "fooBar_put",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": [
          {
            "bearerAuth": []
          },
          {}
        ]
      }
    },
    "/webhook/callback": {
      "post": {
        "operationId": "webhookCallback",
        "responses": {
          "default": {
            "description": "Response from the API's handler."
          }
        },
        "security": []
      }
    }
  }
}

export default {
  ...openApiDocument,
  servers: [{ url: config.serverUrl }],
}
