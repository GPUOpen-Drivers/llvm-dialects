#!/usr/bin/env bash
##
 #######################################################################################################################
 # Copyright (c) 2024 Advanced Micro Devices, Inc. All Rights Reserved.
 #
 # Licensed under the Apache License, Version 2.0 (the "License");
 # you may not use this file except in compliance with the License.
 # You may obtain a copy of the License at
 #
 #     http://www.apache.org/licenses/LICENSE-2.0
 #
 # Unless required by applicable law or agreed to in writing, software
 # distributed under the License is distributed on an "AS IS" BASIS,
 # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 # See the License for the specific language governing permissions and
 # limitations under the License.
 #######################################################################################################################
 ##


# Update the driver and clone a specified version of dialects.
set -e

# Sync the repos. Replace the base llvm-dialects with a freshly checked-out one.
cat /vulkandriver/build_info.txt
(cd /vulkandriver && repo sync -c --no-clone-bundle -j$(nproc))
git -C /vulkandriver/drivers/llpc/imported/llvm-dialects remote add origin https://github.com/"$DIALECTS_REPO_NAME".git
git -C /vulkandriver/drivers/llpc/imported/llvm-dialects fetch origin +"$DIALECTS_REPO_SHA":"$DIALECTS_REPO_REF" --update-head-ok
git -C /vulkandriver/drivers/llpc/imported/llvm-dialects checkout "$DIALECTS_REPO_SHA"
git -C /vulkandriver/drivers/llpc/imported/llvm-dialects submodule update --init
