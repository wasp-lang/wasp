// @ts-nocheck

// This file is used from user-land and the import paths below are relative to the
// user's project dir, and not the SDK:
import { MainPage } from './src/MainPage'

export const routesMapping = {
  RootRoute: MainPage,
} as const;
