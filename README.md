# Chisel Base64

This is a module for [Chisel](chisel-lang.org/) that allows the ability to encode/decode [Base64](https://en.wikipedia.org/wiki/Base64) data.

> Base64 is a group of binary-to-text encoding schemes that represent binary data (more specifically, a sequence of 8-bit bytes) in sequences of 24 bits that can be represented by four 6-bit Base64 digits. [1](https://en.wikipedia.org/wiki/Base64)

## Authors

* Farid Zakaria <fmzakari@ucsc.edu>
* Connor Masterson <ccmaster@ucsc.edu>

## What is implemented?
* Single clock base64 encode
* Generator config for controlling the 64 characters
* Testing for corner cases
* Base64 encoding support with padding
* Generator specification to control bytes per clock (Base64Wrapper)
* Decode

## What remains?
* Try to integrate ScalaCheck
* CRC calculation

## How to?

### Test thhe code
```console
scripts/test
```

### Autoformat the code
```console
scripts/format
```
