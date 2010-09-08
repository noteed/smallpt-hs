all: bench profile smallpt-hs

profile.prof: profile
	./profile +RTS -p

smallpt.o: cbits/smallpt.cpp
	g++ -O3 cbits/smallpt.cpp -c

bench: smallpt.o bench.hs Smallpt/Storable.hs Smallpt/Unboxed.hs Smallpt/Mutable.hs
	ghc --make -O2 \
	bench.hs smallpt.o -o bench -lstdc++

profile: smallpt.o profile.hs Smallpt/Storable.hs Smallpt/Unboxed.hs Smallpt/Mutable.hs
	ghc --make -O2 \
	-prof -auto-all \
	profile.hs smallpt.o -o profile -lstdc++

smallpt-hs: smallpt-hs.hs
	ghc --make -O2 -XForeignFunctionInterface smallpt-hs.hs -o smallpt-hs

clean:
	rm -rf *.o *.hi Smallpt/*.o Smallpt/*.hi smallpt.o bench profile smallpt-hs
