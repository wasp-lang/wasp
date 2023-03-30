import { Request, Response, NextFunction } from "express";
import { RateLimiterMemory } from "rate-limiter-flexible";

// There are two limiters
// - rateLimiter: 10 requests per second
// - oncePerMinuteLimiter: 2 requests per minute

const rateLimiter = new RateLimiterMemory({
  points: 10,
  duration: 1,
})

const oncePerMinuteLimiter = new RateLimiterMemory({
  points: 2,
  duration: 60,
})

const oncePerMinuteRoutes = [
  '/auth/email/signup',
  '/auth/email/request-password-reset',
]

export function rateLimiterMiddleware() {
  return function rateLimit(req: Request, res: Response, next: NextFunction) {
    const limiter = oncePerMinuteRoutes.includes(req.path)
      ? oncePerMinuteLimiter
      : rateLimiter

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
  
