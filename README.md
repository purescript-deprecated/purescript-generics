# purescript-generics

[![Latest release](http://img.shields.io/github/release/purescript/purescript-generics.svg)](https://github.com/purescript/purescript-generics/releases)
[![Build status](https://travis-ci.org/purescript/purescript-generics.svg?branch=master)](https://travis-ci.org/purescript/purescript-generics)

Generic programming.

## Usage

```
bower install purescript-generics
```

The methods in the `Generic` type class can be derived in versions >= 0.7.3 of the PureScript compiler with the following syntax:

``` purescript
derive instance genericMyType :: Generic MyType
```

There are some example usages of the library [in the tests](test/Main.purs).

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-generics).
