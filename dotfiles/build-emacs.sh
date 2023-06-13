sudo apt install libjansson-dev libjansson4
CFLAGS=-O2 CXXFLAGS=-O2 ./configure --with-x-toolkit=gtk3 --with-rsvg --with-png --with-jpeg --with-sqlite3 --with-cairo --with-xml2 --with-json --with-tree-sitter --with-threads --with-modules --with-x
