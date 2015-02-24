# Huffman

A small toy Haskell encoder using Huffman trees. Made for the fun of implementing trees in Haskell; also, why not?

## Using

Make sure you have the Haskell platform installed (GHC and cabal are prerequisites.)

Clone the repository to your filesystem.

In the huffman directory run:
```
cabal install
cd example
make
cat txt/lorem_ipsum.txt | ./Main > txt/lorem_ipsum_compressed
hexdump -C txt/lorem_ipsum_compressed
```

Cool!
