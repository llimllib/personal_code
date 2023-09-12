I had the idea yesterday that I want a better "git status". Instead of:

```
<...snip>
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git restore <file>..." to discard changes in working directory)
	modified:   wt-status.c
<...snip>
```

I want to see the "diff --stat" info there:

```
<...snip>
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git restore <file>..." to discard changes in working directory)
	modified:   wt-status.c     1+ 0-
<...snip>
```

To compile git, I [cloned it from
here](https://github.com/git/git/tree/master/builtin) and had to add homebrew's
header path and library path to my environment with:

```
export C_INCLUDE_PATH=/opt/homebrew/include/
export LIBRARY_PATH=/opt/homebrew/lib/
```

- status lives in `wt-status.c`
    - function `wt_longstatus_print_change_data` is the function that prints an updated file's name, where I'd like to add diffstat
    - is actually called from `builtin/commit.c`
        - `cmd_commit` is what actually constructs the status struct
- diff lives in `diff.c`
    - there's some docs about how to call it in `diff.h` - search for "Calling Sequence"
    - `DIFF_FORMAT_DIFFSTAT` signals that we want a diffstat
    - To create a diffstat struct, populate it, and print it, it does:
    ```c
    struct diff_queue_struct diff_queued_diff;
    void diff_flush(struct diff_options *options) {
        // diff_queued_diff is a module-level variable
        struct diff_queue_struct *q = &diff_queued_diff;
        // <snip lots>
        struct diffstat_t diffstat;
        compute_diffstat(options, &diffstat, q);
        show_stats(&diffstat, options);
    ```

That was a neat dive, but I think probably implementing this from a higher level makes more sense
