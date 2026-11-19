#!/usr/bin/env node
//MISE description="Gets the current version of waspc from the Haskell project."
//MISE quiet=true
/// <reference types="node" />

import { getWaspcVersion } from "../waspc/tools/get-waspc-version.ts";

process.stdout.write(getWaspcVersion());
