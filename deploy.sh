#! /bin/bash

# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout hakyll

# Build new files
stack exec site rebuild

# Get previous files
git fetch --all
git checkout -b master --track origin/master

# Overwrite existing files with new files
git add _site

# Commit
git commit -m "Publish"

# Remove everything else
git filter-branch -f --subdirectory-filter _site/ HEAD

# Push
git push -f origin master:master

# Restore
git checkout hakyll
git branch -D master
git stash pop
