##
## EPITECH PROJECT, 2024
## ASM-MiniLibC
## File description:
## Makefile
##

## Config
NAME 		= 			imageCompressor

BONUS_NAME 	=

EXEC_PATH   =		   	$(shell stack path --local-install-root)

DOC_PATH	=			$(shell stack path --local-doc-root)

## Colors
GREEN 		= 			@/bin/echo -e "\x1b[32m $1\x1b[0m"
RED 		= 			@/bin/echo -e "\x1b[31m $1\x1b[0m"

## Rules
all: 		$(NAME)	clean

$(NAME):
		stack build
		cp $(EXEC_PATH)/bin/$(NAME)-exe $(NAME)
		$(call GREEN,"Build Done ✅!")
clean:
		@stack clean

fclean: 	clean
		@rm -f $(NAME)
		@make -C bonus fclean
		@rm -f $(BONUS_NAME)
		$(call RED,"Clean Done ❌!")

re: 		fclean all

tests_run:
		@stack test --coverage
		@stack hpc report --all --destdir=test/coverage

easteregg:
		@make -C bonus
		@cp bonus/$(BONUS_NAME) $(BONUS_NAME)

doc:
		@stack haddock
		@cp -r $(DOC_PATH) doc
