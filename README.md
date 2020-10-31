# hack-assembler

Hack Assembler for [Nand2Tetris](https://www.nand2tetris.org/project06). 
This was created following [this specification](https://b1391bd6-da3d-477d-8c01-38cdf774495a.filesusr.com/ugd/44046b_b73759b866b249a0b3a715bf5a18f668.pdf).

## Running the Assembler

Run this command to assemble an `.asm` into a `.hack` file.
The `.hack` file will be created in the same directory as the original `.asm` file.
```
sbt "run example/Max.asm"
```

Additional `.asm` files can be found in the [test resources](./src/test/resources/testdata).