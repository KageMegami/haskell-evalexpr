##
## EPITECH PROJECT, 2017
## Makefile
## File description:
## Basic Makfile
##

NAME = funEvalExpr

all:	$(NAME)
	stack build --copy-bins --local-bin-path .
	mv $(NAME)-exe $(NAME)

$(NAME):	$(OBJ)

clean:
	stack clean
	rm .stack-work $(NAME).cabal -rf

fclean: clean
	rm -f $(NAME)

re: fclean all

runtest:
	stack test


