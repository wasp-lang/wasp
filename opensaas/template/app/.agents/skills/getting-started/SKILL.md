---
name: getting-started
description: Get started with your Open SaaS project — fetches docs, checks Wasp installation, and helps you start your database and app.
user_invocable: true
---

# Getting Started with Open SaaS

Follow these steps in order:

## Step 1: Fetch the Documentation Map

Fetch the Open SaaS documentation map from the [LLMs.txt index](https://docs.opensaas.sh/llms.txt). This contains raw markdown file GitHub URLs of all documentation sections.

## Step 2: Fetch the Getting Started Guide

From the documentation map, find the "Getting Started" guide URL (a raw.githubusercontent.com URL) and fetch its full contents. Use this as the source of truth for all instructions below.

## Step 3: Check Wasp Installation

Run `wasp version` to check if Wasp is installed and which version is available.

- **If Wasp is installed:** Confirm the version to the user and continue to Step 4.
- **If Wasp is NOT installed:** Walk the user through the installation instructions from the Getting Started guide for their OS (detect the OS from the environment). After installation, verify with `wasp version` before continuing.

## Step 4: Offer to Help Start the App

Ask the user if they'd like help starting their managed PostgreSQL database and Wasp app according to the instructions in the Getting Started guide.

If yes, walk them through the steps from the guide (starting the database, running migrations, starting the dev server, etc.). If they run into any issues, troubleshoot using the guide's content.
