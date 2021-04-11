@g++ -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -DUNICODE -D_UNICODE -O2 -fomit-frame-pointer %1 %2 %3 Delta.cpp -oDelta.exe -lstdc++ -lole32 -s
:: -DSTAT "-DPROFILING=1" "-DPROFILING=2" "-DPROFILING=3" "-DPROFILING=4"
