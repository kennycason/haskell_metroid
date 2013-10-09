#!/bin/bash

rm Metroid

ghc -threaded *.hs

rm -f *.hi
rm -f *.o

./Metroid
