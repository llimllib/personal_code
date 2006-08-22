INCLUDE := /lib/modules/`uname -r`/build
obj-m   := char_device.o
		
default:
	make -C ${INCLUDE} SUBDIRS=`pwd` modules

.PHONY: clean

clean:
	rm -f *.o *.ko *.mod.c
