#!/bin/bash
set -e  # Exit on error

FILENAME=$(npm pack --silent)
npm install -g "./$FILENAME"
rm "./$FILENAME"