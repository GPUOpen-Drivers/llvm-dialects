#
# Dockerfile for llvm-dialects Continuous Integration.
# Sample invocation:
#    docker build .                                                                                       \
#      --file docker/dialects.Dockerfile                                                                  \
#      --build-arg AMDVLK_IMAGE=amdvlkadmin/amdvlk_release_gcc_assertions:nightly                         \
#      --build-arg DIALECTS_REPO_NAME=GPUOpen-Drivers/llvm-dialects                                       \
#      --build-arg DIALECTS_REPO_REF=<GIT_REF>                                                            \
#      --build-arg DIALECTS_REPO_SHA=<GIT_SHA>                                                            \
#      --tag dialects-ci/dialects
#
# Required arguments:
# - AMDVLK_IMAGE: Base image name for prebuilt amdvlk
# - DIALECTS_REPO_NAME: Name of the llvm-dialects repository to clone
# - DIALECTS_REPO_REF: ref name to checkout
# - DIALECTS_REPO_SHA: SHA of the commit to checkout
#

# Resume build from the specified image.
ARG AMDVLK_IMAGE
FROM "$AMDVLK_IMAGE"

ARG DIALECTS_REPO_NAME
ARG DIALECTS_REPO_REF
ARG DIALECTS_REPO_SHA

# Use bash instead of sh in this docker file.
SHELL ["/bin/bash", "-c"]

# Copy helper scripts into container.
COPY docker/*.sh /vulkandriver/

# Sync the repos. Replace the base repo with a freshly checked-out one.
RUN /vulkandriver/update-dialects.sh

# Build llvm-dialects.
WORKDIR /vulkandriver/builds/ci-build
RUN source /vulkandriver/env.sh \
    && cmake --build . --target llvm-dialects-tblgen

# Run the lit test suite.
RUN source /vulkandriver/env.sh \
    && cmake --build . --target check-llvm-dialects -- -v

# Run the unit tests suite.
RUN source /vulkandriver/env.sh \
    && cmake --build . --target check-llvm-dialects-units -v
