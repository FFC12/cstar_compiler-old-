#!/usr/bin/bash
cd build/
cmake ..
make -j8
./CSTAR ../examples/tests/var00.cstar
