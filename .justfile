initialized_marker := ".initialized"
initialized := path_exists(initialized_marker)


_default:
    just --list


# Initialize repository for building
init:
    #!/bin/sh
    if ! {{initialized}}; then
        opam switch create -y "." --deps-only --package=ocaml-variants.4.14.0+options,ocaml-option-flambda
        eval $(opam env)

        opam update
        opam install -y --deps-only .
        opam install -y dune.3.6.1 ocaml-lsp-server
        opam install -y vector.1.0.0 batteries.3.5.1
        touch {{initialized_marker}};
    fi

# Cleans the directory, removes any built files
@clean:
    eval $(opam env); dune clean
    rm {{initialized_marker}}

# Build all solutions
@build: init
    eval $(opam env); dune build

# Run a specific puzzle
@run puzzle: init
    eval $(opam env); cd {{puzzle}}; OCAMLRUNPARAM=b dune exec ./puzzle{{puzzle}}.exe
