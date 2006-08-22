.data  /* beginning of data segment */

/* hi_temp data item */
        .type hi_temp,@object  /* declare as data object */
        .size hi_temp,1         /* declare size in bytes */
hi_temp:
        .byte 0x92      /* set value */

/* lo_temp data item */
        .type lo_temp,@object
        .size lo_temp,1
lo_temp:
        .byte 0x52

/* av_temp data item */
        .type av_temp,@object
        .size av_temp,1
av_temp:
        .byte 0

/* segment registers set up by linked code */
/* beginning of text(code) segment */
.text
        .align 4        /* set 4 double-word alignment */
.globl main             /* make main global for linker */
        .type main,@function    /* declare main as a function */
main:
        pushl %ebp  /* function requirement */
        movl %esp,%ebp /* function requirement */
        movb hi_temp,%al
        addb lo_temp,%al
        movb $0,%ah
        adcb $0,%ah
        movb $2,%bl
        idivb %bl
        movb %al,av_temp
        leave       /* function requirement */
        ret         /* function requirement */
