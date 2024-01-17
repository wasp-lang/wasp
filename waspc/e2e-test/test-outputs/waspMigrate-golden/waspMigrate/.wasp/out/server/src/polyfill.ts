// This is a polyfill for Node.js 18 webcrypto API so Lucia can use it
// for random number generation.

import { webcrypto } from "node:crypto";

// @ts-ignore
globalThis.crypto = webcrypto as Crypto;
