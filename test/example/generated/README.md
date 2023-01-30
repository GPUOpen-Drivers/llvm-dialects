This directory contains checked-in versions of the auto-generated C++ code for
the example dialect.

It is *only* used by an automated test which diffs the checked-in version
against the version that was generated as part of the build process.

There is no need for you to try to keep the auto-generated C++ code the same
when making changes to llvm-dialects. In fact, many or even most useful changes
will necessarily cause some change in the auto-generated C++ code. In that case,
simply overwrite this checked-in version with the one from the build directory.
The point is to make it easier for code reviewers to understand the effect of
your changes on the generated code.
