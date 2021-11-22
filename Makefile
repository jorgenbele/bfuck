.PHONY: run-compiler run-interpreter

SMLC=mlton
SMLCARGS=-codegen native

SMLI=smlnj
SMLIARGS=

SRCS=intrp.sml

CFLAGS=-Ofast -march=native -mtune=native

clean:
	rm intrp

intrp: intrp.sml
	$(SMLC) $(SMLCARGS) intrp.sml

# .PHONY: interpret-mandelbrot
# interpret-mandelbrot:
# 	$(SMLI) $(SMLARGS) intrp.sml mandelbrot.bf

.PHONY: intrp-mandelbrot
intrp-mandelbrot: intrp
	$(SMLI) $(SMLARGS) intrp.sml -i mandelbrot.bf

# Compiles and runs the mandelbrot program
.PHONY: compile-mandelbrot
compile-mandelbrot: intrp
	./intrp -c mandelbrot.bf > mandelbrot.c
	$(CC) $(CFLAGS) mandelbrot.c -o mandelbrot

# Compiles a brainfuck compiler written in brainfuck
#  and runs the hello world program on that
.PHONY: dbfi
dbfi: intrp
	./intrp -c dbfi.b > dbfi.c
	$(CC) $(CFLAGS) dbfi.c -o dbfi
	./dbfi < bitwidth.b
