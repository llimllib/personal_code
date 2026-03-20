# brew install gnucobol
a_cobol: a.cobol
	cobc -x a.cobol -o a_cobol

.PHONY: run_cobol
run_cobol: a_cobol
	time ./a_cobol < input.txt
