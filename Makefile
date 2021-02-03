All: kompilator

Lexer.hs: Lexer.x
	alex -o Lexer.hs Lexer.x 

Parser.hs: Lexer.x
	happy Parser.y

kompilator: Lexer.hs Parser.hs
	ghc -o kompilator main.hs

clear:
	rm -f *.hi *.o
	rm -f kompilator