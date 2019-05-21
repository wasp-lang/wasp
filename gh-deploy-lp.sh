#!/bin/sh
git push origin `git subtree split --prefix landing-page master`:gh-pages --force
