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
@cli *FLAGS:
  cabal run . -- {{FLAGS}}

# cabal repl
@repl:
  cabal repl

# print env vars
@vars:
  python3 jsetup-utils/check_env.py jambhalarch

hls-bin := `which haskell-language-server`

# generate .env file from template
@mk-env:
  if [ ! -f .env ]; then \
    cp jsetup-utils/.env-template .env; \
  fi

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

@fmt:
  find . -name '*.nix' -not -path './dist*/*' -not -path './haddock/*' -exec nixpkgs-fmt {} +
  cabal-fmt -i {{ cabal-sources }}
  fourmolu {{ fourmolu-exts }} -m inplace {{ haskell-sources }}