#!/bin/sh

#ghc -O2 -fllvm -o Main Main.hs --make -fforce-recomp -rtsopts

ghc -v0  -fforce-recomp -Odph -O2 -funbox-strict-fields -fllvm -fasm -optc-O1 -optc-03 -o Main Main.hs --make -rtsopts

#ghc -prof -auto -O2 -fllvm -o Main Main.hs --make -fforce-recomp -rtsopts

# -prof, -auto for profiling (note no profiling cause ISO has System.Random)

#ghc - threaded -O2 -v3 -fllvm Fibber.hs --make -fforce-recomp -keep-tmp-files

#ghc -O2 -v3 -fasm Fibber.hs --make -fforce-recomp -keep-tmp-files

#ghc -O2 -fvia-C -optc-O3 Fibber.hs --make -fforce-recomp -keep-tmp-files