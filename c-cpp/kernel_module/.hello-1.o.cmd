cmd_/c/code/c-cpp/kernel_module/hello-1.o := gcc -Wp,-MD,/c/code/c-cpp/kernel_module/.hello-1.o.d -nostdinc -iwithprefix include -D__KERNEL__ -Iinclude  -D__KERNEL__ -Iinclude  -Wall -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -fno-common -pipe -mregparm=3 -msoft-float -fno-builtin-sprintf -fno-builtin-log2 -fno-builtin-puts -mpreferred-stack-boundary=2 -march=i686 -Iinclude/asm-i386/mach-default -Os -fomit-frame-pointer -g     -DMODULE -DKBUILD_BASENAME=hello_1 -DKBUILD_MODNAME=hello_1 -c -o /c/code/c-cpp/kernel_module/.tmp_hello-1.o /c/code/c-cpp/kernel_module/hello-1.c

deps_/c/code/c-cpp/kernel_module/hello-1.o := \
  /c/code/c-cpp/kernel_module/hello-1.c \
  include/linux/init.h \
    $(wildcard include/config/modules.h) \
    $(wildcard include/config/hotplug.h) \
  include/linux/config.h \
    $(wildcard include/config/h.h) \
  include/linux/compiler.h \
  include/linux/compiler-gcc3.h \
  include/linux/compiler-gcc.h \
  include/linux/module.h \
    $(wildcard include/config/modversions.h) \
    $(wildcard include/config/module/unload.h) \
    $(wildcard include/config/kallsyms.h) \
  include/linux/sched.h \
    $(wildcard include/config/hugetlb/page.h) \
    $(wildcard include/config/smp.h) \
    $(wildcard include/config/numa.h) \
    $(wildcard include/config/security.h) \
    $(wildcard include/config/preempt.h) \
  include/asm/param.h \
  include/linux/capability.h \
  include/linux/types.h \
    $(wildcard include/config/uid16.h) \
  include/linux/posix_types.h \
  include/linux/stddef.h \
  include/asm/posix_types.h \
  include/asm/types.h \
    $(wildcard include/config/highmem64g.h) \
    $(wildcard include/config/lbd.h) \
  include/linux/spinlock.h \
    $(wildcard include/config/debug/spinlock.h) \
  include/linux/preempt.h \
  include/linux/linkage.h \
  include/asm/linkage.h \
    $(wildcard include/config/x86/alignment/16.h) \
  include/linux/thread_info.h \
  include/linux/bitops.h \
  include/asm/bitops.h \
  include/asm/thread_info.h \
  include/asm/page.h \
    $(wildcard include/config/x86/use/3dnow.h) \
    $(wildcard include/config/x86/pae.h) \
    $(wildcard include/config/highmem4g.h) \
    $(wildcard include/config/x86/4g/vm/layout.h) \
    $(wildcard include/config/discontigmem.h) \
  include/asm/processor.h \
    $(wildcard include/config/x86/pc9800.h) \
    $(wildcard include/config/x86/high/entry.h) \
    $(wildcard include/config/mk8.h) \
    $(wildcard include/config/mk7.h) \
  include/asm/vm86.h \
  include/asm/math_emu.h \
  include/asm/sigcontext.h \
  include/asm/segment.h \
  include/asm/cpufeature.h \
  include/asm/msr.h \
  include/asm/system.h \
    $(wildcard include/config/x86/cmpxchg.h) \
    $(wildcard include/config/x86/oostore.h) \
  include/linux/kernel.h \
    $(wildcard include/config/debug/spinlock/sleep.h) \
  /usr/lib/gcc-lib/i386-redhat-linux/3.2.2/include/stdarg.h \
  include/asm/byteorder.h \
    $(wildcard include/config/x86/bswap.h) \
  include/linux/byteorder/little_endian.h \
  include/linux/byteorder/swab.h \
  include/linux/byteorder/generic.h \
  include/asm/bug.h \
  include/linux/cache.h \
  include/asm/cache.h \
    $(wildcard include/config/x86/l1/cache/shift.h) \
  include/linux/threads.h \
    $(wildcard include/config/nr/cpus.h) \
  include/linux/stringify.h \
  include/asm/atomic.h \
  include/linux/timex.h \
    $(wildcard include/config/time/interpolation.h) \
  include/asm/timex.h \
    $(wildcard include/config/melan.h) \
    $(wildcard include/config/x86/tsc.h) \
  include/linux/time.h \
  include/linux/seqlock.h \
  include/asm/div64.h \
  include/linux/jiffies.h \
  include/linux/rbtree.h \
  include/linux/cpumask.h \
  include/asm/cpumask.h \
  include/asm-generic/cpumask.h \
  include/linux/bitmap.h \
  include/linux/string.h \
  include/asm/string.h \
  include/asm-generic/cpumask_up.h \
  include/asm-generic/cpumask_const_value.h \
  include/asm/semaphore.h \
  include/linux/wait.h \
  include/linux/list.h \
  include/linux/prefetch.h \
  include/linux/rwsem.h \
    $(wildcard include/config/rwsem/generic/spinlock.h) \
  include/asm/rwsem.h \
  include/asm/ptrace.h \
  include/asm/mmu.h \
  include/linux/smp.h \
  include/linux/sem.h \
  include/linux/ipc.h \
  include/asm/ipcbuf.h \
  include/asm/sembuf.h \
  include/linux/signal.h \
  include/asm/signal.h \
  include/asm/siginfo.h \
  include/asm-generic/siginfo.h \
  include/linux/securebits.h \
  include/linux/fs_struct.h \
  include/linux/completion.h \
  include/linux/pid.h \
  include/linux/percpu.h \
  include/linux/slab.h \
    $(wildcard include/config/.h) \
  include/linux/gfp.h \
  include/linux/mmzone.h \
    $(wildcard include/config/force/max/zoneorder.h) \
  include/linux/numa.h \
  include/linux/topology.h \
  include/asm/topology.h \
  include/asm-generic/topology.h \
  include/linux/kmalloc_sizes.h \
    $(wildcard include/config/mmu.h) \
    $(wildcard include/config/large/allocs.h) \
  include/asm/percpu.h \
  include/asm-generic/percpu.h \
  include/linux/param.h \
  include/linux/resource.h \
  include/asm/resource.h \
  include/linux/timer.h \
  include/linux/aio.h \
  include/linux/workqueue.h \
  include/linux/aio_abi.h \
  include/asm/current.h \
  include/linux/stat.h \
  include/asm/stat.h \
  include/linux/kmod.h \
    $(wildcard include/config/kmod.h) \
  include/linux/errno.h \
  include/asm/errno.h \
  include/asm-generic/errno.h \
  include/asm-generic/errno-base.h \
  include/linux/elf.h \
  include/asm/elf.h \
  include/asm/user.h \
  include/linux/utsname.h \
  include/asm/local.h \
  include/asm/module.h \
    $(wildcard include/config/m386.h) \
    $(wildcard include/config/m486.h) \
    $(wildcard include/config/m586.h) \
    $(wildcard include/config/m586tsc.h) \
    $(wildcard include/config/m586mmx.h) \
    $(wildcard include/config/m686.h) \
    $(wildcard include/config/mpentiumii.h) \
    $(wildcard include/config/mpentiumiii.h) \
    $(wildcard include/config/mpentium4.h) \
    $(wildcard include/config/mk6.h) \
    $(wildcard include/config/mcrusoe.h) \
    $(wildcard include/config/mwinchipc6.h) \
    $(wildcard include/config/mwinchip2.h) \
    $(wildcard include/config/mwinchip3d.h) \
    $(wildcard include/config/mcyrixiii.h) \
    $(wildcard include/config/mviac3/2.h) \

/c/code/c-cpp/kernel_module/hello-1.o: $(deps_/c/code/c-cpp/kernel_module/hello-1.o)

$(deps_/c/code/c-cpp/kernel_module/hello-1.o):
