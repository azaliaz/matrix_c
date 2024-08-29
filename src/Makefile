
CC = gcc
TESTFLAGS = -lcheck -g
CFLAGS = -Wall -Wextra -Werror -std=c11 -I -lcheck
TESTFLAGS2 = -lcheck -lm -lpthread -lrt -g
GCOVFLAGS = -fprofile-arcs -ftest-coverage

all: test

s21_matrix.a : s21_matrix.c
	$(CC) $(CFLAGS) -g -std=c11 -c s21_matrix.c
	ar rcs s21_matrix.a *.o
	rm -rf *.o

test: clean unit_tests.c s21_matrix.c
	$(CC) $(CFLAGS) unit_tests.c s21_matrix.c -o test $(TESTFLAGS) -lm
	./test

gcov_report:
	$(CC) $(GCOVFLAGS) $(CFLAGS) unit_tests.c s21_matrix.c -o test $(TESTFLAGS) -lm
	./test
	lcov -t "test" -o test.info -c -d .
	genhtml -o report test.info
	open report/index.html

rebuild: clean all

clean: 
	rm -rf *.o test *.a a.out *.gcno *.gcda test.dSYM *.info report

style:
	cp ../materials/linters/.clang-format .
	clang-format *.c *.h -i
	rm .clang-format