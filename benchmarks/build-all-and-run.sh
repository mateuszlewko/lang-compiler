if ! [ -x "$(command -v hyperfine)" ]; then
    printf "Error: hyperfine not found. Please go to \n\
       https://github.com/sharkdp/hyperfine\n       and install it.\n";
    exit 1;
fi

if [ ! -d ./benchmarks ]; then
    echo "Error: This script needs to be run from main project folder 
(monoml-compiler/)";
    exit 1;
fi

# Java 
echo 'Compiling Java...'
javac benchmarks/Java/Sum.java 

# Haskell
echo 'Compiling Haskell...'
ghc benchmarks/Haskell/sumPoly.hs -XMagicHash -O2
ghc benchmarks/Haskell/sumMono.hs -XMagicHash -O2

# MLton 
echo 'Compiling MLton...'
mlton benchmarks/MLton/sumMono.sml

# MonoML
echo 'Compiling MonoML...'
make 
_build/default/langc/langc.exe benchmarks/MonoML/sumMono.la -o benchmarks/MonoML/sumMono
_build/default/langc/langc.exe benchmarks/MonoML/sumPoly.la -o benchmarks/MonoML/sumPoly

echo 'Initalizing benchmark with hyperfine...'

hyperfine -m 30 --warmup 10 benchmarks/{MonoML/sum{Mono,Poly},MLton/sumMono} \
benchmarks/Haskell/sum{Mono,Poly} \
'(cd benchmarks/Java/ && java Sum -- 100000000 poly)' \
'(cd benchmarks/Java/ && java Sum -- 100000000 mono)' \
'smlnj benchmarks/SmlNJ/sumPoly.sml'
