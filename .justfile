initialized_marker := ".initialized"
initialized := path_exists(initialized_marker)


_default:
    just --list


# Initialize repository for building
init:
    #!/bin/sh
    if ! {{initialized}}; then
        opam switch create -y "." --deps-only --package=ocaml-variants.5.0.0~rc1+options,ocaml-option-flambda
        eval $(opam env --set-switch --switch=.)

        opam update
        opam install -y --deps-only .
        opam install -y dune.3.6.1 ocaml-lsp-server ocamlformat
        opam install -y vector.1.0.0 batteries.3.6.0
        touch {{initialized_marker}};
    fi

# Cleans the directory, removes any built files
@clean:
    eval $(opam env --set-switch --switch=.); dune clean
    rm {{initialized_marker}}

# Build all solutions
@build: init
    eval $(opam env --set-switch --switch=.); dune build --profile=release

# Run a specific puzzle
@run puzzle: init
    eval $(opam env --switch=.); cd {{puzzle}}; OCAMLRUNPARAM=b dune exec --profile=release ./puzzle{{puzzle}}.exe
