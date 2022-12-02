initialized_marker := ".initialized"
initialized := path_exists(initialized_marker)


default:
    just --list


# Initialize repository for building
init:
    #!/bin/sh
    if ! {{initialized}}; then
        sudo apt -y install pkg-config

        opam switch create -y "." --deps-only --package=ocaml-variants.4.14.0+options,ocaml-option-flambda
        eval $(opam env)

        opam update
        opam install -y --deps-only .
        opam install -y vector.1.0.0 batteries.3.5.1
        opam install -y dune ocaml-lsp-server
        touch {{initialized_marker}};
    fi

# Cleans the directory, removes any built files
@clean:
    eval $(opam env); dune clean
    rm {{initialized_marker}}

# Build the HC800 emulator and firmware
@build: init
    eval $(opam env); dune build

@run puzzle: build
    eval $(opam env); cd {{puzzle}}; dune exec ./puzzle{{puzzle}}.exe
