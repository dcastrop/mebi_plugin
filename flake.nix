{
  description = "MEBI: a Rocq plugin for mechanised bisimilarity proofs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      # aarch64-darwin is offered on the assumption nothing here is
      # Linux-specific; it is untested. x86_64-darwin is deliberately absent:
      # nixpkgs 26.11 dropped support for it, and listing it makes
      # `nix flake check --all-systems` fail outright.
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          # Nix provides opam and the system libraries that opam packages compile
          # against. The OCaml/Rocq packages themselves are managed by opam and
          # pinned exactly by rocq-mebi.opam.locked -- not here.
          #
          # curl, unzip and bwrap already come in the nixpkgs `opam` closure, and
          # the C toolchain, patch and tar come from stdenv, so none are listed.
          nativeBuildInputs = with pkgs; [
            opam
            pkg-config # opam's conf-pkg-config
            git # opam VCS pins, and `dune subst` in the opam build
            gnumake # the `rocq makefile` build path
          ];

          # If an opam package fails to build citing a missing system library,
          # add it here. These two are required by the current lock:
          buildInputs = with pkgs; [
            gmp # conf-gmp -> zarith -> rocq-runtime
            zlib # conf-zlib
          ];

          # opam probes for "external dependencies" through the system package
          # manager. On NixOS it finds none and offers to run nix-build for gmp
          # and pkg-config -- which aborts the bootstrap even though this shell
          # already provides them. Everything opam needs is in buildInputs
          # above, so tell it not to look.
          OPAMNODEPEXTS = "1";

          shellHook = ''
            mebi_bootstrap='opam switch create . --locked --deps-only --yes && opam install . --locked --deps-only --with-dev-setup --yes'

            # Test for the binary rather than just _opam/, so a half-built switch
            # is caught too.
            if [ ! -x "$PWD/_opam/bin/rocq" ]; then
              echo "mebi: no local opam switch in ./_opam (or it is incomplete)."
              echo "mebi: create it with:"
              echo "    $mebi_bootstrap"

              # nix-direnv *executes* this hook, and direnv can inherit the
              # terminal's stdin -- a bare `read` there would hang every `cd`
              # into the project. direnv sets DIRENV_IN_ENVRC while evaluating
              # .envrc, so only prompt outside it, and only on a tty.
              if [ -z "''${DIRENV_IN_ENVRC:-}" ] && [ -t 0 ]; then
                printf 'mebi: create it now? this takes a while [y/N] '
                read -r mebi_reply
                case "$mebi_reply" in
                  [yY]*) eval "$mebi_bootstrap" ;;
                  *) echo "mebi: skipped." ;;
                esac
                unset mebi_reply
              else
                echo "mebi: run 'nix develop' for an interactive prompt."
              fi
            fi

            unset mebi_bootstrap
          '';
        };
      });
    };
}
