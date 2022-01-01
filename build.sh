#!/usr/bin/bash
cd build/
cmake ..
make -j8
./CSTAR ../examples/syntax.cstar
