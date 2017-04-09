#! /bin/bash

# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout hakyll

# Build new files
./dist/build/site/site rebuild

# Get previous files
git fetch --all
git checkout -b master --track origin/master

# Overwrite existing files with new files
cp -a _site/* .

# Commit
git add -A
git commit -m "Publish"

# Push
git push origin master:master

# Restoration
git checkout hakyll
git branch -D master
git stash pop
