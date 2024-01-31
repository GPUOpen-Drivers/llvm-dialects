llvm-dialects
=============

Overview
========
The LLVM Dialects library allows defining "dialects" on top of the LLVM IR
"substrate" in a TableGen-based DSL that looks, on purpose, very much like the
DSL used to define MLIR dialects.

Its main user is [LLPC](https://github.com/GPUOpen-Drivers/llpc). We welcome
other interested parties. Development of llvm-dialects happens on GitHub,
using Issues and Pull Requests.

LLVM Compatibility
==================
The `dev` branch is generally expected to work with (a recent version of) the
`main` branch of the llvm-project repository. No promises are made about
compatibility with any specific LLVM version.

Components
==========

- lib/Dialect: a helper library that allows the creation of basic dialects (in
  the MLIR sense) for LLVM IR
- lib/TableGen: common code for interpreting TableGen records that define a
  dialect
- utils/llvm-dialects-tblgen: a standalone CLI tool which reads a .td dialect
  definition and outputs C++ header and source files

Building
========

The are multiple ways to combine `llvm` and `llvm-dialects` into a build process.
Common options are:

- Build `llvm-dialects` stand-alone against an installed `llvm`.
- Build both `llvm-dialects` and `llvm` as part of some larger build process,
  e.g. with both being submodules of the enclosing project.
- Build `llvm-dialects` as part of an `llvm` build. After checking out
  `llvm-dialects` in `llvm/tools` and re-running CMake, the `llvm_dialects`
  target (and other `llvm-dialects` targets) is available.

Testing
=======
Run the `check-llvm-dialects` target to run a suite of automated tests.
Run the `check-llvm-dialects-lit` target to run only the lit tests, and 
the `check-llvm-dialects-units` to run only the unit tests.
