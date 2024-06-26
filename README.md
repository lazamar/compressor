# Compressor

This is an example lossless data compression program.

The goal is to illustrate an implementation of Huffman coding in Haskell and how it can be used to compress binary files.
As such the code is as clear and simple as possible.

It uses a semi-static zero-order byte-based model and a Huffman coder.
It can compress arbitrary binary files and uses constant memory for encoding and decoding.

The constant memory overhead shows the power of laziness at work.
Despite none of the functions using explicit stream processing, files are lazily read and written to disk as streams.

Compression works in two passes. The first records byte frequencies and builds the model. The second uses the model to code the resulting bytes.

Run it:

```
# Compile
$ ghc -O2 Main.hs -o main

# compress
$ ./main WarAndPeace.txt WarAndPeace.txt.compressed
Done.

# decompress
$ ./main decompress WarAndPeace.txt.compressed WarAndPeace.txt.expanded
Done.

# Result. 40% decrease in size.
$ du -h WarAndPeace*
3.2M    WarAndPeace.txt
1.9M    WarAndPeace.txt.compressed
3.2M    WarAndPeace.txt.expanded
````
