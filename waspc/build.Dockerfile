# syntax=docker.io/docker/dockerfile:1

FROM docker.io/library/alpine AS ghc-base

RUN --mount=type=cache,target=/var/cache/apk <<EOF
  apk update
  # Dependency list from https://www.haskell.org/ghcup/install/#linux-alpine
  apk add binutils-gold curl gcc g++ gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev perl pkgconfig tar xz
EOF

ARG GHC_VERSION=8.10.7
RUN \
  --mount=type=cache,target=/root/.ghcup/cache \
  --mount=type=tmpfs,target=/root/.ghcup/logs \
<<EOF
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
  source ~/.ghcup/env
  ghcup config set cache true
  ghcup install cabal --set latest
  ghcup install ghc --set ${GHC_VERSION}
EOF

ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:$PATH"



FROM ghc-base AS builder

RUN --mount=type=cache,target=/var/cache/apk <<EOF
  apk update
  # Build script dependencies
  apk add bash nodejs npm
  # Cabal dependencies
  apk add zlib-dev zlib-static
EOF


WORKDIR /work
COPY --link . .

RUN \
  --mount=type=cache,target=/root/.cache \
  --mount=type=cache,target=/root/.local/state/cabal/store \
  --mount=type=cache,target=/root/.npm/_cacache \
  --mount=type=tmpfs,target=/tmp \
<<EOF
  set -euxo pipefail
  cabal update
  ./run build:all:static
  mkdir -p artifacts
  ./tools/make_binary_package.sh "artifacts/wasp.tar.gz"
EOF



FROM scratch AS output

COPY --from=builder /work/artifacts/wasp.tar.gz /artifacts/wasp.tar.gz
