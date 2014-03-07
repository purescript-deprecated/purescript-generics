all: lib test

lib:
	mkdir -p js/Data
	psc src/Data/Generics.purs.hs \
	  -o js/Data/Generics.js \
	  -e js/Data/Generics.e.purs.hs \
	  --module Data.Generics --tco --magic-do

test:
	psc src/Data/Generics.purs.hs examples/Expr.purs.hs \
	  -o js/Expr.js \
	  --main --module Main --tco --magic-do

docs:
	docgen src/Data/Generics.purs.hs > docs/README.md

