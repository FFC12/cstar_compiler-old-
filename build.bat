@echo off
DEL build
mkdir build
cmake --no-warn-unused-cli -DCMAKE_EXPORT_COMPILE_COMMANDS:BOOL=TRUE -DCMAKE_BUILD_TYPE:STRING=Debug -DCMAKE_C_COMPILER:FILEPATH=C:\TDM-GCC-64\bin\x86_64-w64-mingw32-gcc.exe -DCMAKE_CXX_COMPILER:FILEPATH=C:\TDM-GCC-64\bin\x86_64-w64-mingw32-g++.exe "-Hc:/CStar" "-Bc:/CStar/build" -G "MinGW Makefiles"
cmake --build "c:/CStar/build" --config Debug --target CSTAR -j 10 --
:: cls
c:/CStar/build/CSTAR.exe
