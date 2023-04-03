import { Request, Response, NextFunction } from "express";
import { RateLimiterMemory } from "rate-limiter-flexible";

// 3 requests per minute
const limiter = new RateLimiterMemory({
  points: 3,
  duration: 60,
})

const rateLimitedRoutes = [
  '/auth/email/signup',
  '/auth/email/request-password-reset',
]

export function rateLimiterMiddleware() {
  return function rateLimit(req: Request, res: Response, next: NextFunction) {
    if (!rateLimitedRoutes.includes(req.path)) {
      return next()
    }

    limiter
      .consume(req.ip)
      .then(() => {
        next()
      })
      .catch(() => {
        res.status(429).json({ message: 'Too Many Requests' })
      })
  }
}
  
