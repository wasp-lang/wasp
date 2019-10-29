#!/bin/sh
GIT_ROOT=`git rev-parse --show-toplevel`
DIR_TO_DEPLOY=`pwd`
DIR_TO_DEPLOY_REL_TO_GIT_ROOT="${DIR_TO_DEPLOY#"$GIT_ROOT"/}"
cd $GIT_ROOT
SUBTREE_MASTER_REF=`git subtree split --prefix "$DIR_TO_DEPLOY_REL_TO_GIT_ROOT" master`
git push origin "$SUBTREE_MASTER_REF":gh-pages --force
