{{={= =}=}}
import { Router } from "express";

import waspServerConfig from '../../../config.js';

import { ProviderConfig } from "../types.js";
import { emailSender } from '../../../email/index.js';
import type { EmailFromField } from '../../../email/core/types.js';

import { sign, verify } from '../../../core/auth.js'

const fromField: EmailFromField = {
    name: '{= fromField.name =}',
    email: '{= fromField.email =}',
};

const config: ProviderConfig = {
    id: "{= providerId =}",
    displayName: "{= displayName =}",
    createRouter() {
        const router = Router();

        router.post('/login', async (req, res) => {
            console.log('login', req.body);
            const token = await sign("123")
          
            return res.json({ token })
        });
        router.post('/signup', async (req, res) => {
            const verifyEmailToken = await sign("123");
            const verifyEmailUrl = `${waspServerConfig.backendUrl}/auth/email/verify-email?token=${verifyEmailToken}`;
            emailSender.send({
                from: fromField,
                to: req.body.email,
                subject: 'Verify your email',
                text: `Click the link below to verify your email: ${verifyEmailUrl}`,
                html: `
                    <p>Click the link below to verify your email</p>
                    <a href="${verifyEmailUrl}">Verify email</a>
                `,
            });

            console.log('signup', req.body);
            const token = await sign("123");
          
            return res.json({ token });
        });
        router.post('/request-password-reset', async (req, res) => {
            const token = await sign("123");
            const clientRoute = '{= passwordResetClientRoute =}';
            const passwordResetUrl = `${waspServerConfig.frontendUrl}${clientRoute}?token=${token}`;
            emailSender.send({
                from: fromField,
                to: req.body.email,
                subject: 'Reset your password',
                text: `Click the link below to reset your password: ${passwordResetUrl}`,
                html: `
                    <p>Click the link below to reset your password</p>
                    <a href="${passwordResetUrl}">Reset password</a>
                `,
            });

            console.log('request-password-reset', req.body);
            res.json({ success: true });
        });
        router.post('/reset-password', async (req, res) => {
            const token = req.body.token;
            try {
                await verify(token);
            } catch (e) {
                // TODO: redirect to error page
                res.status(400).json({ error: 'Invalid token' });
                return;
            }
            res.json({ success: true });
        });
        router.get('/verify-email', async (req, res) => {
            const token = req.query.token;
            try {
                await verify(token);
            } catch (e) {
                // TODO: redirect to error page
                res.status(400).json({ error: 'Invalid token' });
                return;
            }

            const onVerifySuccessRedirectTo = '{= onVerifySuccessRedirectTo =}';
            const redirectUrl = `${waspServerConfig.frontendUrl}${onVerifySuccessRedirectTo}?result=success`;
            res.redirect(redirectUrl);
        });

        return router;
    },
}

export default config;
