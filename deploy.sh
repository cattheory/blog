#!/usr/bin/env bash

DEPLOY="temp/"
#REMOTE="git@github.com:joom/joom.github.io.git"
REMOTE="https://github.com/joom/joom.github.io.git"
SITE="_site"

COMMIT=$(git log -1 HEAD --pretty=format:%H)
SHA=${COMMIT:0:8}
echo $SHA

echo "Creating deploy directory..."
rm -rf $DEPLOY
mkdir $DEPLOY
cd $DEPLOY

echo "Initializing git..."
git init -q
git checkout --orphan master -q
git remote add origin $REMOTE
cd "../"

echo "Building site..."
stack exec site build > /dev/null
cp -r $SITE/* $DEPLOY
cp -r files/* $DEPLOY
cd $DEPLOY

echo "Pushing site to git..."
git add --all
git commit -m "generated from $SHA"
git push origin master --force -q

echo "Finished deploying!"
