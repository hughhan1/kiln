ARG NODE_IMAGE
FROM ${NODE_IMAGE} AS runtime
ARG NODE_VERSION
ARG PACKAGE_MANAGER
ARG PACKAGE_MANAGER_VERSION
RUN test "$(node --version)" = "v${NODE_VERSION}" && \
    if [ "$PACKAGE_MANAGER" != npm ] || [ "$(npm --version)" != "$PACKAGE_MANAGER_VERSION" ]; then npm install --global "${PACKAGE_MANAGER}@${PACKAGE_MANAGER_VERSION}"; fi && \
    test "$("$PACKAGE_MANAGER" --version)" = "$PACKAGE_MANAGER_VERSION"

FROM runtime AS builder
RUN apt-get update && apt-get install -y --no-install-recommends build-essential python3 git openssh-client curl ca-certificates pkg-config && rm -rf /var/lib/apt/lists/*
