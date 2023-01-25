/*
 * Copyright (c) 2022 Advanced Micro Devices, Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "llvm-dialects/TableGen/Operations.h"

using namespace llvm;
using namespace llvm_dialects;

SmallVector<OpNamedValue> OpClass::getFullArguments() const {
  SmallVector<OpNamedValue> args;
  if (superclass)
    args = superclass->getFullArguments();
  args.insert(args.end(), arguments.begin(), arguments.end());
  return args;
}

unsigned OpClass::getNumFullArguments() const {
  if (superclass)
    return superclass->getNumFullArguments() + arguments.size();
  return arguments.size();
}

SmallVector<OpNamedValue> Operation::getFullArguments() const {
  SmallVector<OpNamedValue> args;
  if (superclass)
    args = superclass->getFullArguments();
  args.insert(args.end(), arguments.begin(), arguments.end());
  return args;
}

unsigned Operation::getNumFullArguments() const {
  if (superclass)
    return superclass->getNumFullArguments() + arguments.size();
  return arguments.size();
}
