# Compressor

This is an example data compression program.

The goal is to illustrate an implementation of Huffman coding in Haskell and its application in compressing binary files.
As such the code is as clear and simple as possible.

It uses a semi-static zero-order byte-based model and a Huffman coder.
It can compress any kind of file and uses constant memory for encoding and decoding.

The constant memory overhead shows the power of laziness at work.
Despite none of the functions using explicit stream processing, files are lazily read and written to disk as a stream.

Compression works in two passes. The first records byte frequencies and builds the model. The second uses the model to code the resulting bytes.
