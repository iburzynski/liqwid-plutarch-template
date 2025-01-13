# direnv allow
@allow:
  direnv allow

# direnv reload
@reload:
  direnv reload

# codium editor
@code DIRECTORY='.':
  if [ "{{ DIRECTORY }}" = "." ] || [ -d "{{ DIRECTORY }}" ]; then \
    if [ "${VIM_MODE}" = 'true' ]; then \
      codium {{ DIRECTORY }}; \
    else codium {{ DIRECTORY }} --disable-extension asvetliakov.vscode-neovim; \
    fi \
  else echo "Invalid directory: {{ DIRECTORY }}"; \
  fi

# jamb cli
@cli *OPTS:
  cabal run . -- {{ OPTS }}

# cabal repl
@repl:
  cabal repl

# print env vars
@vars:
  python3 jsetup-utils/check_env.py jambhalarch

hls-bin := `which haskell-language-server`

# generate .env file from template
@mk-env:
  mk-env.sh

# create HLS symlink
@link-hls:
  if [ -n "{{ hls-bin }}" ]; then \
    ln -s -f "{{ hls-bin }}" .vscode/haskell-language-server.link; \
  else echo "haskell-language-server not found!"; \
  fi

hoogle-port := "8080"

# start hoogle server
@hoogle:
  echo "Starting Hoogle server @ http://localhost:{{ hoogle-port }}..."
  hoogle server --local -p {{ hoogle-port }} >/dev/null

haskell-sources := "$(git ls-tree -r HEAD --full-tree --name-only | grep '.\\+\\.hs')"
cabal-sources := "$(git ls-tree -r HEAD --full-tree --name-only | grep '.\\+\\.cabal')"
fourmolu-exts := "-o -XQuasiQuotes -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms -o -XOverloadedRecordDot"

# format *.nix, *.cabal, and *.hs files
@fmt:
  find . -name '*.nix' -not -path './dist*/*' -not -path './haddock/*' -exec nixpkgs-fmt --check {} + > /dev/null 2>&1
  cabal-fmt -i {{ cabal-sources }} > /dev/null 2>&1
  fourmolu {{ fourmolu-exts }} -m inplace {{ haskell-sources }} > /dev/null 2>&1

# run cardano-ez-installer
@cardano-install:
  python3 cardano-ez-installer/main.py

node-network-id := env_var('CARDANO_NODE_NETWORK_ID')
node-network := if node-network-id == "1" { "preprod" } else if node-network-id == "2" { "preview" } else { "mainnet" }
cardano-path := env_var('CARDANO_PATH')
network-path := cardano-path / node-network
socket-path := cardano-path / "node.socket"
config-path := network-path / "config"
node-port := "1337"

# run cardano-node
@node:
  cardano-node run \
  --topology {{ config-path }}/topology.json \
  --database-path {{ network-path }}/db \
  --socket-path {{ socket-path }} \
  --port {{ node-port }} \
  --config {{ config-path }}/config.json