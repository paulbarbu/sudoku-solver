all:
	ghc --make Main.hs -o sudokusolver.exe

clean:
	rm -rf *.hi *.o *.exe
