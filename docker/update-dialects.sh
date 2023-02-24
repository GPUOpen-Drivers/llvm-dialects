#!/usr/bin/env bash
# Update the driver and clone a specified version of dialects.
set -e

# Sync the repos. Replace the base llvm-dialects with a freshly checked-out one.
cat /vulkandriver/build_info.txt
(cd /vulkandriver && repo sync -c --no-clone-bundle -j$(nproc))
git -C /vulkandriver/drivers/llpc/imported/llvm-dialects remote add origin https://github.com/"$DIALECTS_REPO_NAME".git
git -C /vulkandriver/drivers/llpc/imported/llvm-dialects fetch origin +"$DIALECTS_REPO_SHA":"$DIALECTS_REPO_REF" --update-head-ok
git -C /vulkandriver/drivers/llpc/imported/llvm-dialects checkout "$DIALECTS_REPO_SHA"
git -C /vulkandriver/drivers/llpc/imported/llvm-dialects submodule update --init
