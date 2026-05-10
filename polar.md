# Intégration Polar

Pour valider correctement les webhooks Polar en développement local (ngrok), vous devez contourner le parseur JSON par défaut de Wasp qui altère la signature cryptographique.

### Configuration du Middleware (Raw Body)

Dans votre fichier `main.wasp`, déclarez votre route de webhook avec une configuration de middleware personnalisée :

```wasp
api polarWebhook {
  fn: import { polarWebhook } from "@src/webhooks/polar.js",
  httpRoute: (POST, "/webhook/polar"),
  middlewareConfigFn: import { polarWebhookMiddleware } from "@src/webhooks/middleware.js"
}
