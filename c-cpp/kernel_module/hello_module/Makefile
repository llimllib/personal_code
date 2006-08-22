INCLUDE := /lib/modules/`uname -r`/build
obj-m   := hello_mod.o
		
default:
	make -C ${INCLUDE} SUBDIRS=`pwd` modules

.PHONY: clean

clean:
	rm -f *.o *.ko *.mod.c
