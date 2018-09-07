BUILD_DIR=THESIS_BUILD

echo "Removing directory $BUILD_DIR"
rm -rf $BUILD_DIR

mkdir $BUILD_DIR

cp thesis/thesis.pdf $BUILD_DIR/Praca_Licencjacka.pdf
cp -r ./benchmarks $BUILD_DIR/benchmarks
cp -r ./compiler $BUILD_DIR/compiler
cp ./external.c $BUILD_DIR/external.c
cp -r 


