" genutils: Useful buffer, file and window related functions.
" Author: Hari Krishna Dara <hari_vim at yahoo dot com>
" Last Change: 09-May-2006 @ 10:44
" Requires: Vim-6.3, multvals.vim(3.5)
" Version: 1.20.0
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Acknowledgements:
"     - The GetNextWinnrInStack() function is based on the WinStackMv()
"       function posted by Charles E. Campbell, Jr. on vim mailing list on Jul
"       14, 2004.
"     - The CommonPath() function is based on the thread,
"       "computing relative path" on Jul 29, 2002.
"     - The ShowLinesWithSyntax() function is based on a posting by Gary
"       Holloway (gary at castandcrew dot com) on Jan, 16 2002.
"     - Robert Webb for the original "quick sort" algorithm from eval.txt.
"     - Peit Delport's (pjd at 303 dot za dot net) for his original BISort()
"       algorithm on which the BinInsertSort() and BinInsertSort2() functions
"       are based on.
" Download From:
"     http://www.vim.org/script.php?script_id=197
" Description:
"   - Read the "Documentation With Function Prototypes" section below.
"   - Functions MakeArgumentString(), MakeArgumentList() and CreateArgString()
"     to work with and pass variable number of arguments to other functions.
"     There is also an ExtractFuncListing() function that is used by the above
"     functions to create snippets (see also breakpts.vim, ntservices.vim and
"     ntprocesses.vim for interesting ideas on how to use this function).
"   - Misc. window/buffer related functions, NumberOfWindows(),
"     FindBufferForName(), MoveCursorToWindow(), MoveCurLineToWinLine(),
"     SetupScratchBuffer(), MapAppendCascaded()
"   - Save/Restore all the window height/width settings to be restored later.
"   - Save/Restore position in the buffer to be restored later. Works like the
"     built-in marks feature, but has more to it.
"   - AddNotifyWindowClose() to get notifications *after* a window with the
"     specified buffer has been closed or the buffer is unloaded. The built-in
"     autocommands can only notify you *before* the window is closed. You can
"     use this with the Save/Restore window settings feature to restore the
"     dimensions of existing windows, after your window is closed (just like
"     how Vim does while closing help windows). See selectbuf.vim or
"     perforce.vim for examples.
"     There is also a test function called RunNotifyWindowCloseTest() that
"     demos the usage (you need to uncomment RunNotifyWindowCloseTest and
"     NotifyWindowCloseF functions).
"   - ShowLinesWithSyntax() function to echo lines with syntax coloring.
"   - ShiftWordInSpace(), CenterWordInSpace() and
"     AlignWordWithWordInPreviousLine() utility functions to move words in the
"     space without changing the width of the field. A GetSpacer() function to
"     return a spacer of specified width.
"   - A quick-sort functions QSort() that can sort a buffer contents by range
"     and QSort2() that can sort any arbitrary data and utility compare
"     methods.  Binary search functions BinSearchForInsert() and
"     BinSearchForInsert2() to find the location for a newline to be inserted
"     in an already sorted buffer or arbitrary data.
"   - ExecMap function has now been separated as a plugin called execmap.vim.
"   - A sample function to extract the scriptId of a script.
"   - New CommonPath() function to extract the common part of two paths, and
"     RelPathFromFile() and RelPathFromDir() to find relative paths (useful
"     HTML href's). A side effect is the CommonString() function to find the
"     common string of two strings.
"   - UnEscape() and DeEscape() functions to reverse and Escape() to compliment
"     what built-in escape() does. There is also an EscapeCommand() function
"     to escape external command strings.
"   - Utility functions CurLineHasSign() and ClearAllSigns() to fill in the
"     gaps left by Vim.
"   - GetVimCmdOutput() function to capture the output of Vim built-in
"     commands, in a safe manner.
"   - OptClearBuffer() function to clear the contents and undo history of the
"     current buffer in an optimal manner. Ideal to be used when plugins need
"     to refresh their windows and don't care about preserving the current
"     contents (which is the most usual case).
"   - GetPreviewWinnr() function.
"   - Functions to have persistent data, PutPersistentVar() and
"     GetPersistentVar(). You don't need to worry about saving in files and
"     reading them back. To disable, set g:genutilsNoPersist in your vimrc.
"   - A function to emulate the default Vim behavior for |timestamp| changes.
"     It also provides hooks to get call backs before and after handling the
"     default FileChangedShell autocommand (effectively splitting it into a
"     Pre and a Post event). Suggested usage is to use AddToFCShellPre() or
"     AddToFCShell() and either install a default event handling mechanism for
"     all files by calling DefFCShellInstall() or create your own autocommand on
"     a matching pattern to call DefFileChangedShell() function. Most useful
"     for the source control plugins to conditionally reload a file, while
"     being able to default to the Vim's standard behavior of asking the user.
"     See perforce.vim for usage examples.
"
"   Function Prototypes:
"     The types in prototypes of the functions mimic Java.
"     This is just a full list for a quick reference, see
"       "Documentation With Function Prototypes" for more information on the
"       functions.
"
"   String  MakeArgumentString(...)
"   String  MakeArgumentList(...)
"   String  CreateArgString(String argList, String sep, ...)
"   void    DebugShowArgs(...)
"   String  ExtractFuncListing(String funcName, String hLines, String tLines)
"   int     NumberOfWindows()
"   int     FindBufferForName(String fileName)
"   String  GetBufNameForAu(String bufName)
"   void    MoveCursorToWindow(int winno)
"   void    MoveCurLineToWinLine(int winLine)
"   void    CloseWindow(int winnr, boolean force)
"   void    MarkActiveWindow()
"   void    RestoreActiveWindow()
"   void    IsOnlyVerticalWindow()
"   void    IsOnlyHorizontalWindow()
"   int     GetNextWinnrInStack(char dir)
"   int     GetLastWinnrInStack(char dir)
"   void    MoveCursorToNextInWinStack(char dir)
"   void    MoveCursorToLastInWinStack(char dir)
"   void    OpenWinNoEa(String openWinCmd)
"   void    CloseWinNoEa(int winnr, boolean force)
"   void    SetupScratchBuffer()
"   void    CleanDiffOptions()
"   boolean ArrayVarExists(String varName, int index)
"   void    MapAppendCascaded(String lhs, String rhs, String mapMode)
"   void    SaveWindowSettings()
"   void    RestoreWindowSettings()
"   void    ResetWindowSettings()
"   void    SaveWindowSettings2(String sid, boolean overwrite)
"   void    RestoreWindowSettings2(String scripid)
"   void    ResetWindowSettings2(String scripid)
"   void    SaveVisualSelection(String scripid)
"   void    RestoreVisualSelection(String scripid)
"   void    SaveSoftPosition(String id)
"   void    RestoreSoftPosition(String id)
"   void    ResetSoftPosition(String id)
"   void    SaveHardPosition(String id)
"   void    RestoreHardPosition(String id)
"   void    ResetHardPosition(String id)
"   int     GetLinePosition(String id)
"   int     GetColPosition(String id)
"   boolean IsPositionSet(String id)
"   String  CleanupFileName(String fileName)
"   boolean OnMS()
"   boolean PathIsAbsolute(String path)
"   boolean PathIsFileNameOnly(String path)
"   void    AddNotifyWindowClose(String windowTitle, String functionName)
"   void    RemoveNotifyWindowClose(String windowTitle)
"   void    CheckWindowClose()
"   void    ShowLinesWithSyntax() range
"   void    ShiftWordInSpace(int direction)
"   void    CenterWordInSpace()
"   void    QSort(String cmp, int direction) range
"   void    QSort2(int start, int end, String cmp, int direction,
"                  String accessor, String swapper, String context)
"   int     BinSearchForInsert(int start, int end, String line, String cmp,
"                              int direction)
"   int     BinSearchForInsert2(int start, int end, line, String cmp,
"                               int direction, String accessor, String context)
"   void    BinInsertSort(String cmp, int direction) range
"   void    BinInsertSort2(int start, int end, String cmp, int direction,
"                  String accessor, String mover, String context)
"   String  CommonPath(String path1, String path2)
"   String  CommonString(String str1, String str2)
"   String  RelPathFromFile(String srcFile, String tgtFile)
"   String  RelPathFromDir(String srcDir, String tgtFile)
"   String  Roman2Decimal(String str)
"   String  Escape(String str, String chars)
"   String  UnEscape(String str, String chars)
"   String  DeEscape(String str)
"   String  EscapeCommand(String cmd, String args, String pipe)
"   int     GetShellEnvType()
"   String  ExpandStr(String str)
"   String  QuoteStr(String str)
"   boolean CurLineHasSign()
"   void    ClearAllSigns()
"   String  UserFileComplete(String ArgLead, String CmdLine, String CursorPos,
"                            String smartSlash, String searchPath)
"   String  UserFileExpand(String fileArgs)
"   String  GetVimCmdOutput(String cmd)
"   void    OptClearBuffer()
"   int     GetPreviewWinnr()
"   void    PutPersistentVar(String pluginName, String persistentVar,
"                            String value)
"   void    GetPersistentVar(String pluginName, String persistentVar,
"                            String default)
"   void    AddToFCShellPre(String funcName)
"   void    RemoveFromFCShellPre(String funcName)
"   void    AddToFCShell(String funcName)
"   void    RemoveFromFCShell(String funcName)
"   void    DefFCShellInstall()
"   void    DefFCShellUninstall()
"   boolean DefFileChangedShell()
"   void    SilentSubstitute(String pat, String cmd)
"   void    SilentDelete(String pat)
"   void    SilentDelete(String range, String pat)
"   String  GetSpacer(int width)
"
" Documentation With Function Prototypes:
" -----------------------
" Execute the function return value to create a local variable called
"   'argumentString' containsing all your variable number of arguments that
"   are passed to your function, such a way that it can be passed further down
"   to another function as arguments.
" Uses __argCounter and __nextArg local variables, so make sure you don't have
"   variables with the same name in your function. If you want to change the
"   name of the resultant variable from the default 'argumentString' to
"   something else, you can pass the new name as an argument.
" Ex:
"   fu! s:IF(...)
"     exec MakeArgumentString()
"     exec "call Impl(1, " . argumentString . ")"
"   endfu
"
" String  MakeArgumentString(...)
" -----------------------
" Execute the function return value to create a local variable called
"   'argumentList' containsing all your variable number of arguments that
"   are passed to your function, such a way that it can manipulated as an
"   array list separated by commas before passing further down to another
"   function as arguments. Once manipulated, the string can be passed to
"   CreateArgString() function to make an argument string which can be
"   used just like the 'argumentString' as mentioned in the documentation on
"   MakeArgumentString(). To manipulate 'argumentList' you can use
"   multvals.vim script.
" Unlike MakeArgumentString(), this doesn't preserve the argument types
"   (string vs number).
" Uses __argCounter and __argSeparator local variables, so make sure you don't
"   have variables with the same name in your function. You can change the
"   name of the resultant variable from the default 'argumentList' to
"   something else, by passing the new name as the first argument. You can
"   also pass in a second optional argument which is used as the argument
"   separator instead of the default ','. You need to make sure that the
"   separator string itself can't occur as part of arguments, or use a
"   sequence of characters that is hard to occur, as separator.
" Ex: 
"   fu! s:IF(...)
"     exec MakeArgumentList()
"     exec "call Impl(1, " . CreateArgString(argumentList, ',') . ")"
"   endfu
"
" String  MakeArgumentList(...)
" -----------------------
" Useful to collect arguments into a soft-array (see multvals on vim.sf.net)
"   and then pass them to a function later.
" You should make sure that the separator itself doesn't exist in the
"   arguments. You can use the return value same as the way argumentString
"   created by the "exec g:makeArgumentString" above is used. If the separator
"   is a pattern, you should pass in an optional additional argument, which
"   is an arbitrary string that is guaranteed to match the pattern, as a
"   sample separator (see multvals.vim for details).
" Usage:
"     let args = 'a b c' 
"     exec "call F(" . CreateArgString(args, ' ') . ")"
"
" String  CreateArgString(String argList, String sep, ...)
" -----------------------
" Useful function to debug passing arguments to functions. See exactly what
"   you would receive on the other side.
" Ex: :exec 'call DebugShowArgs('. CreateArgString("a 'b' c", ' ') . ')' 
"
" void    DebugShowArgs(...)
" -----------------------
" This function returns the body of the specified function ( the name should be
"   complete, including any scriptid prefix in case of a script local
"   function), without the function header and tail. You can also pass in the
"   number of additional lines to be removed from the head and or tail of the
"   function.
"
" String  ExtractFuncListing(String funcName, String hLines, String tLines)
" -----------------------
" -----------------------
" Return the number of windows open currently.
"
" int     NumberOfWindows()
" -----------------------
" Returns the buffer number of the given fileName if it is already loaded.
" The fileName argument is treated literally, unlike the bufnr() which treats
"   the argument as a filename-pattern. The function first escape all the
"   |filename-pattern| characters before passing it to bufnr(). It should work
"   in most of the cases, except when backslashes are used in non-windows
"   platforms, when the result could be unpredictable.
"
" Note: The function removes protections for "#%" characters because, these
"   are special characters on Vim commandline, and so are usually escaped
"   themselves, but bufnr() wouldn't like them.
"
" int     FindBufferForName(String fileName)
" -----------------------
" Returns the transformed buffer name that is suitable to be used in
"   autocommands.
"
" String  GetBufNameForAu(String bufName)
" -----------------------
" Given the window number, moves the cursor to that window.
"
" void    MoveCursorToWindow(int winno)
" -----------------------
" Moves the current line such that it is going to be the nth line in the window
"   without changing the column position.
"
" void    MoveCurLineToWinLine(int winLine)
" -----------------------
" Closes the given window and returns to the original window. It the simplest,
" this is equivalent to:
"
"   let curWin = winnr()
"   exec winnr 'wincmd w'
"   close
"   exec curWin 'wincmd w'
"
" But the function keeps track of the change in window numbers and restores
" the current window correctly. It also restores the previous window (the
" window that the cursor would jump to when executing "wincmd p" command).
" This is something that all plugins should do while moving around in the
" windows, behind the scenes.
"
" Pass 1 to force closing the window (:close!).
"
" void    CloseWindow(int winnr, boolean force)
" -----------------------
" Remembers the number of the current window as well as the previous-window
" (the one the cursor would jump to when executing "wincmd p" command). To
" determine the window number of the previous-window, the function temporarily
" jumps to the previous-window, so if your script intends to avoid generating
" unnecessary window events, consider disabling window events before calling
" this function (see :h 'eventignore').
"
" void    MarkActiveWindow()
" -----------------------
" Restore the cursor to the window that was previously marked as "active", as
" well as its previous-window (the one the cursor would jump to when executing
" "wincmd p" command). To restore the window number of the previous-window,
" the function temporarily jumps to the previous-window, so if your script
" intends to avoid generating unnecessary window events, consider disabling
" window events before calling this function (see :h 'eventignore').
"
" void    RestoreActiveWindow()
" -----------------------
" Returns 1 if the current window is the only window vertically.
"
" void    IsOnlyVerticalWindow()
" -----------------------
" Returns 1 if the current window is the only window horizontally.
"
" void    IsOnlyHorizontalWindow()
" -----------------------
" Returns the window number of the next window while remaining in the same
"   horizontal or vertical window stack (or 0 when there are no more). Pass
"   hjkl characters to indicate direction.
"   Usage:
"     let wn = GetNextWinnrInStack('h') left  window number in stack.
"     let wn = GetNextWinnrInStack('l') right window number in stack.
"     let wn = GetNextWinnrInStack('j') upper window number in stack.
"     let wn = GetNextWinnrInStack('k') lower window number in stack.
"
" int     GetNextWinnrInStack(char dir)
" -----------------------
" Returns the window number of the last window while remaining in the same
"   horizontal or vertical window stack (or 0 when there are no more, or it is
"   already the last window). Pass hjkl characters to indicate direction.
"   Usage:
"     let wn = GetLastWinnrInStack('h') leftmost  window number in stack.
"     let wn = GetLastWinnrInStack('l') rightmost window number in stack.
"     let wn = GetLastWinnrInStack('j') top       window number in stack.
"     let wn = GetLastWinnrInStack('k') bottom    window number in stack.
"
" int     GetLastWinnrInStack(char dir)
" -----------------------
" Move cursor to the next window in stack. See GetNextWinnrInStack() for more
"   information.
"
" void    MoveCursorToNextInWinStack(char dir)
" -----------------------
" Move cursor to the last window in stack. See GetLastWinnrInStack() for more
"   information.
"
" void    MoveCursorToLastInWinStack(char dir)
" -----------------------
" This function, which stands for "execute the given command that creates a
"   window, while disabling the 'equalalways' setting", is a means for plugins
"   to create new windows without disturbing the existing window dimensions as
"   much as possible. This function would not be required if 'equalalways' is
"   not set by the user. Even if set, the below code, though intuitive,
"   wouldn't work:
"       let _equalalways = &equalalways
"       set noequalalways
"       " open window now.
"       let &equalalways = _equalalways
"
" The problem is that while restoring the value of equalalways, if the user
"   originally had it set, Vim would immediately try to equalize all the
"   window dimensions, which is exactly what we tried to avoid by setting
"   'noequalalways'. The function works around the problem by temporarily
"   setting 'winfixheight' in all the existing windows and restoring them
"   after done.
"   Usage:
"     call OpenWinNoEa('sb ' pluginBuf)
"
" Note: The function doesn't catch any exceptions that are generated by the
"   operations, so it is advisable to catch them by the caller itself.
"
" void    OpenWinNoEa(String openWinCmd)
" -----------------------
" This is for the same purpose as described for OpenWinNoEa() function, except
"   that it is used to close a given window. This is just a convenience
"   function.
"
" void    CloseWinNoEa(int winnr, boolean force)
" -----------------------
" Turn on some buffer settings that make it suitable to be a scratch buffer.
"
" void    SetupScratchBuffer()
" -----------------------
" Turns off those options that are set by diff to the current window.
"   Also removes the 'hor' option from scrollopt (which is a global option).
" Better alternative would be to close the window and reopen the buffer in a
"   new window. 
"
" void    CleanDiffOptions()
" -----------------------
" This function is an alternative to exists() function, for those odd array
"   index names for which the built-in function fails. The var should be
"   accessible to this functions, so it shouldn't be a local or script local
"   variable.
"     if ArrayVarExists("array", id)
"       let val = array{id}
"     endif
"
" boolean ArrayVarExists(String varName, int index)
" -----------------------
" If lhs is already mapped, this function makes sure rhs is appended to it
"   instead of overwriting it. If you are rhs has any script local functions,
"   make sure you use the <SNR>\d\+_ prefix instead of the <SID> prefix (or the
"   <SID> will be replaced by the SNR number of genutils script, instead of
"   yours).
" mapMode is used to prefix to "oremap" and used as the map command. E.g., if
"   mapMode is 'n', then the function call results in the execution of noremap
"   command.
"
" void    MapAppendCascaded(String lhs, String rhs, String mapMode)
" -----------------------
" -----------------------
" Saves the heights and widths of the currently open windows for restoring
"   later.
"
" void    SaveWindowSettings()
" -----------------------
" Restores the heights of the windows from the information that is saved by
"  SaveWindowSettings(). Works only when the number of windows haven't changed
"  since the SaveWindowSettings is called.
"
" void    RestoreWindowSettings()
" -----------------------
" Reset the previously saved window settings using SaveWindowSettings.
"
" void    ResetWindowSettings()
" -----------------------
" Same as SaveWindowSettings, but uses the passed in id to create a
"   private copy for the calling script. Pass in a unique id to avoid
"   conflicting with other callers. If overwrite is zero and if the settings
"   are already stored for the passed in sid, it will overwrite previously
"   saved settings.
"
" void    SaveWindowSettings2(String sid, boolean overwrite)
" -----------------------
" Same as RestoreWindowSettings, but uses the passed in id to get the
"   settings. The settings must have been previously saved using this
"   id. Call ResetWindowSettings2() to explicitly reset the saved
"   settings.
"
" void    RestoreWindowSettings2(String id)
" -----------------------
" Reset the previously saved window settings using SaveWindowSettings2.
"   Releases the variables.
"
" void    ResetWindowSettings2(String id)
" -----------------------
" -----------------------
" Save the current/last visual selection such that it can be later restored
"   using RestoreVisualSelection(). Pass a unique scripid such that it will
"   not interfere with the other callers to this function. Saved selections
"   are not associated with the window so you can later restore the selection
"   in any window, provided there are enough lines/columns.
"
" void    SaveVisualSelection(String id)
" -----------------------
" Restore the visual selection that was previuosly saved using
"   SaveVisualSelection().
"
" void    RestoreVisualSelection(String id)
" -----------------------
" -----------------------
" This method tries to save the hard position along with the line context This
"   is like the vim builtin marker. Pass in a unique id to avoid
"   conflicting with other callers.
"
" void    SaveSoftPosition(String id)
" -----------------------
" Restore the cursor position using the information saved by the previous call
"   to SaveSoftPosition. This first calls RestoreHardPosition() and then
"   searches for the original line first in the forward direction and then in
"   the backward and positions the cursor on the line if found. If the
"   original line is not found it still behaves like a call to
"   RestoreHardPosition. This is similar to the functionality of the built-in
"   marker, as Vim is capable of maintaining the marker even when the line is
"   moved up or down. However, if there are identical lines in the buffer and
"   the original line has moved, this function might get confused.
"
" void    RestoreSoftPosition(String id)
" -----------------------
" Reset the previously cursor position using SaveSoftPosition. Releases the
"   variables.
"
" void    ResetSoftPosition(String id)
" -----------------------
" Useful when you want to go to the exact (line, col), but marking will not
"   work, or if you simply don't want to disturb the marks. Pass in a unique
"   id.
"
" void    SaveHardPosition(String id)
" -----------------------
" Restore the cursor position using the information saved by the previous call
"   to SaveHardPosition. 
"
" void    RestoreHardPosition(String id)
" -----------------------
" Reset the previously cursor position using SaveHardPosition. Releases the
"   variables.
"
" void    ResetHardPosition(String id)
" -----------------------
" Return the line number of the previously saved position for the id.
"   This is like calling line() builtin function for a mark.
"
" int     GetLinePosition(String id)
" -----------------------
" Return the column number of the previously saved position for the id.
"   This is like calling col() builtin function for a mark.
"
" int     GetColPosition(String id)
" -----------------------
" A convenience function to check if a position has been saved (and not reset)
"   using the id given.
"
" boolean IsPositionSet(String id)
" -----------------------
" -----------------------
" Cleanup file name such that two *cleaned up* file names are easy to be
"   compared. This probably works only on windows and unix platforms. Also
"   recognizes UNC paths. Always returns paths with forward slashes only,
"   irrespective of what your 'shellslash' setting is. The return path will
"   always be a valid path for use in Vim, provided the original path itself
"   was valid for the platform (a valid cygwin path after the cleanup will
"   still be valid in a cygwin vim).
"
" String  CleanupFileName(String fileName)
" -----------------------
" Returns true if the current OS is any of the Microsoft OSes. Most useful to
"   know if the path separator is "\".
"
" boolean OnMS()
" -----------------------
" Returns true if the given path could be an absolute path. Probably works
"   only on Unix and Windows platforms.
"
" boolean PathIsAbsolute(String path)
" -----------------------
" Returns true if the given path doesn't have any directory components.
"   Probably works only on Unix and Windows platforms.
"
" boolean PathIsFileNameOnly(String path)
" -----------------------
" -----------------------
" Add a notification to know when a buffer with the given name (referred to as
"   windowTitle) is no longer visible in any window. This by functionality is
"   like a BufWinLeavePost event. The function functionName is called back
"   with the title (buffer name) as an argument. The notification gets removed
"   after excuting it, so for future notifications, you need to reregister
"   your function. You can only have one notification for any buffer. The
"   function should be accessible from the script's local context.
"
" void    AddNotifyWindowClose(String windowTitle, String functionName)
" -----------------------
" Remove the notification previously added using AddNotifyWindowClose
"   function.
"
" void    RemoveNotifyWindowClose(String windowTitle)
" -----------------------
" Normally the plugin checks for closed windows for every WinEnter event, but
"   you can force a check at anytime by calling this function.
"
" void    CheckWindowClose()
" -----------------------
" -----------------------
" Displays the given line(s) from the current file in the command area (i.e.,
"   echo), using that line's syntax highlighting (i.e., WYSIWYG).  If no line
"   number is given, display the current line.
" Originally,
"   From: Gary Holloway "gary at castandcrew dot com"
"   Date: Wed, 16 Jan 2002 14:31:56 -0800
"
" void    ShowLinesWithSyntax() range
" -----------------------
" This function shifts the current word in the space without changing the
"   column position of the next word. Doesn't work for tabs.
"
" void    ShiftWordInSpace(int direction)
" -----------------------
" This function centers the current word in the space without changing the
"   column position of the next word. Doesn't work for tabs.
" 
" void    CenterWordInSpace()
" -----------------------
" -----------------------
" Sorts a range of lines in the current buffer, in the given range, using the
"   comparator that is passed in. The comparator function should accept the
"   two lines that needs to be compared and the direction as arguments and
"   return -1, 0 or 1. Pass 1 or -1 for direction to mean asending or
"   descending order. The function should be accessible from the script's
"   local context. The plugin also defines the following comparators that you
"   can use: CmpByString, CmpByStringIgnoreCase, CmpByNumber,
"            CmpByLength, CmpByLineLengthNname, CmpJavaImports
"
" void    QSort(String cmp, int direction) range
" -----------------------
" This is a more generic version of QSort, that will let you provide your own
"   accessor and swapper, so that you can extend the sorting to something
"   beyond the current buffer lines. See multvals.vim plugin for example
"   usage.
"
" The swapper is called with the two indices that need to be swapped, along
" with the context, something like this:
"     function! CustomSwapper(ind1, ind2, context)
"
" This is based up on the sort functions given as part of examples in the
"   eval.txt file, however this rectifies a bug in the original algorithm and
"   makes it generic too.
"
" void    QSort2(int start, int end, String cmp, int direction,
"                String accessor, String swapper, String context)
" -----------------------
" Return the line number where given line can be inserted in the current
"   buffer. This can also be interpreted as the line in the current buffer
"   after which the new line should go.
" Assumes that the lines are already sorted in the given direction using the
"   given comparator.
"
" int     BinSearchForInsert(int start, int end, String line, String cmp,
"                            int direction)
" -----------------------
" A more generic implementation of BinSearchForInsert, which doesn't restrict
"   the search to the current buffer.
"
" int     BinSearchForInsert2(int start, int end, line, String cmp,
"                             int direction, String accessor, String context)
" -----------------------
" Sorts a range of lines in the current buffer, in the given range, using the
"   comparator that is passed in. This function is just like QSort(), except
"   that it uses Peit Delport's "binary insertion sort" algorithm, which is
"   generally much faster than the "quick sort" algorithm.
"
" void    BinInsertSort(String cmp, int direction) range
" -----------------------
" This is a more generic version of BinInsertSort, just like QSort2, that will
"   let you provide your own accessor and mover, so that you can extend the
"   sorting to something beyond the current buffer lines. See multvals.vim
"   plugin for example usage.
"
" The mover is called with the index that needs to be moved and the
"   destination index to which it needs to be moved, along with the context,
"   something like this:
"     function! CustomMover(from, to, context)
"
" void    BinInsertSort2(int start, int end, String cmp, int direction,
"                String accessor, String mover, String context)
" -----------------------
" -----------------------
" Find common path component of two filenames.
"   Based on the thread, "computing relative path".
"   Date: Mon, 29 Jul 2002 21:30:56 +0200 (CEST)
" Ex:
"   CommonPath('/a/b/c/d.e', '/a/b/f/g/h.i') => '/a/b/'
"
" String  CommonPath(String path1, String path2)
" -----------------------
" Find common string component of two strings.
"   Based on the tread, "computing relative path".
"   Date: Mon, 29 Jul 2002 21:30:56 +0200 (CEST)
" Ex:
"   CommonString('abcde', 'abfghi') => 'ab'
"
" String  CommonString(String str1, String str2)
" -----------------------
" Find the relative path of tgtFile from the directory of srcFile.
"   Based on the tread, "computing relative path".
"   Date: Mon, 29 Jul 2002 21:30:56 +0200 (CEST)
" Ex:
"   RelPathFromFile('/a/b/c/d.html', '/a/b/e/f.html') => '../f/g.html'
"
" String  RelPathFromFile(String srcFile, String tgtFile)
" -----------------------
" Find the relative path of tgtFile from the srcDir.
"   Based on the tread, "computing relative path".
"   Date: Mon, 29 Jul 2002 21:30:56 +0200 (CEST)
" Ex:
"   RelPathFromDir('/a/b/c/d', '/a/b/e/f/g.html') => '../../e/f/g.html'
"
" String  RelPathFromDir(String srcDir, String tgtFile)
" -----------------------
" -----------------------
" Convert Roman numerals to decimal. Doesn't detect format errors.
" Originally,
"   From: "Preben Peppe Guldberg" <c928400@student.dtu.dk>
"   Date: Fri, 10 May 2002 14:28:19 +0200
"
" String  Roman2Decimal(String str)
" -----------------------
" -----------------------
" Works like the built-in escape(), except that it escapes the specified
"   characters only if they are not already escaped, so something like
"   Escape('a\bc\\bd', 'b') would give 'a\bc\\\bd'. The chars value directly
"   goes into the [] collection, so it can be anything that is accepted in [].
"
" String  Escape(String str, String chars)
" -----------------------
" Works like the reverse of the builtin escape() function, but un-escapes the
"   specified characters only if they are already escaped (essentially the
"   opposite of Escape()). The chars value directly goes into the []
"   collection, so it can be anything that is acceptable to [].
"
" String  UnEscape(String str, String chars)
" -----------------------
" Works like the reverse of the built-in escape() function. De-escapes all the
"   escaped characters. Essentially removes one level of escaping from the
"   string, so something like: 'a\b\\\\c\\d' would become 'ab\\c\d'.
"
" String  DeEscape(String str)
" ----------------------- 
" Escape the passed in shell command with quotes and backslashes such a way
"   that the arguments reach the command literally (avoids shell
"   interpretations). See the function header for the kind of escapings that
"   are done. The first argument is the actual command name, the second
"   argument is the arguments to the command and third argument is any pipe
"   command that should be appended to the command. The reason the function
"   requires them to be passed separately is that the escaping is minimized
"   for the first and third arguments. It understand the protected spaces in
"   the arguments.
"   Usage:
"     let fullCmd = EscapeCommand('ls', '-u '.expand('%:h'), '| grep xxx')
"   Note:
"     If the escaped command is used on Vim command-line (such as with ":w !",
"     ":r !" and ":!"), you need to further protect '%', '#' and '!' chars,
"     even if they are in quotes, to avoid getting expanded by Vim before
"     invoking external cmd. However this is not required for using it with
"     system() function. The easiest way to escape them is by using the
"     Escape() function as in "Escape(fullCmd, '%#!')".
" String  EscapeCommand(String cmd, String args, String pipe)
" -----------------------
" Returns the global ST_* constants (g:ST_WIN_CMD, g:ST_WIN_SH, g:ST_UNIX)
" based on the values of shell related settings and the OS on which Vim is
" running.
"
" int     GetShellEnvType()
" -----------------------
"
" Expands the string for the special characters. The return value should
"   essentially be what you would see if it was a string constant with
"   double-quotes.
" Ex:
"   ExpandStr('a\tA') => 'a     A'
" String  ExpandStr(String str)
" -----------------------
" Quotes the passed in string such that it can be used as a string expression
" in :execute. It sorrounds the passed in string with single-quotes while
" escaping any existing single-quotes in the string.
"
" String  QuoteStr(String str)
" -----------------------
" -----------------------
" Returns true if the current line has a sign placed.
"
" boolean CurLineHasSign()
" -----------------------
" Clears all signs in the current buffer.
"
" void    ClearAllSigns()
" -----------------------
" -----------------------
" This function is suitable to be used by custom command completion functions
"   for expanding filenames conditionally. The function could based on the
"   context, decide whether to do a file completion or a different custom
"   completion. See breakpts.vim and perforce.vim for examples.
" If you pass non-zero value to smartSlash, the function decides to use
"   backslash or forwardslash as the path separator based on the user settings
"   and the ArgLead, but if you always want to use only forwardslash as the
"   path separator, then pass 0. If you pass in a comma separated list of
"   directories as searchPath, then the file expansion is limited to the files
"   under these directories. This means, you can implement your own commands
"   that don't expect the user to type in the full path name to the file
"   (e.g., if the user types in the command while in the explorer window, you
"   could assume that the path is relative to the directory being viewed). Most
"   useful with a single directory, but also useful in combination with vim
"   'runtimepath' in loading scripts etc. (see Runtime command in
"   breakpts.vim).
"
" String  UserFileComplete(String ArgLead, String CmdLine, String CursorPos,
"                          String smartSlash, String searchPath)
" -----------------------
" This is a convenience function to expand filename meta-sequences in the
"   given arguments just as Vim would have if given to a user-defined command
"   as arguments with completion mode set to "file". Useful
"   if you set the completion mode of your command to anything
"   other than the "file", and later conditionally expand arguments (for
"   characters such as % and # and other sequences such as #10 and <cword>)
"   after deciding which arguments represent filenames/patterns.
"
" String  UserFileExpand(String fileArgs)
" -----------------------
" This returns the output of the vim command as a string, without corrupting
"   any registers. Returns empty string on errors. Check for v:errmsg after
"   calling this function for any error messages.
"
" String  GetVimCmdOutput(String cmd)
" -----------------------
" Clear the contents of the current buffer in an optimum manner. For plugins
" that keep redrawing the contents of its buffer, executing "1,$d" or its
" equivalents result in overloading Vim's undo mechanism. Using this function
" avoids that problem.
"
" void    OptClearBuffer()
" -----------------------
" Returns the window number of the preview window if open or -1 if not.
" int     GetPreviewWinnr()
" -----------------------
" -----------------------
" These functions provide a persistent storage mechanism.
"
"     Example: Put the following in a file called t.vim in your plugin
"     directory and watch the magic. You can set new value using SetVar() and
"     see that it returns the same value across session when GetVar() is
"     called.
"     >>>>t.vim<<<<
"       au VimEnter * call LoadSettings()
"       au VimLeavePre * call SaveSettings()
"       
"       function! LoadSettings()
"         let s:tVar = GetPersistentVar("T", "tVar", "defVal")
"       endfunction
"       
"       function! SaveSettings()
"         call PutPersistentVar("T", "tVar", s:tVar)
"       endfunction
"       
"       function! SetVar(val)
"         let s:tVar = a:val
"       endfunction
"       
"       function! GetVar()
"         return s:tVar
"       endfunction
"     <<<<t.vim>>>>
"
" The pluginName and persistentVar have to be unique and are case insensitive.
"   Ideally called from your VimLeavePre autocommand handler of your plugin.
"   This simply creates a global variable which will be persisted by Vim
"   through viminfo. The variable can be read back in the next session by the
"   plugin using GetPersistentVar() function, ideally from your VimEnter
"   autocommand handler. The pluginName is to provide a name space for
"   different plugins, and avoid conflicts in using the same persistentVar
"   name.
" This feature uses the '!' option of viminfo, to avoid storing all the
"   temporary and other plugin specific global variables getting saved.
"
" void    PutPersistentVar(String pluginName, String persistentVar,
"                          String value)
" -----------------------
" Ideally called from VimEnter, this simply reads the value of the global
"   variable for the persistentVar that is saved in the viminfo in a previous
"   session using PutPersistentVar() and returns it (and default if the variable
"   is not found). It removes the variable from global space before returning
"   the value, so can be called only once. It also means that PutPersistentVar
"   should be called again in the next VimLeavePre if the variable continues
"   to be persisted.
"
" void    GetPersistentVar(String pluginName, String persistentVar,
"                          String default)
" -----------------------
" -----------------------
" These functions channel the FileChangedShell autocommand and extend it to
" create an additional fictitious FileChangedShellPre and FileChangedShellPost
" events.
"
" Add the given noarg function to the list of functions that need to be
"   notified before processing the FileChangedShell event. The function when
"   called can expand "<abuf>" or "<afile>" to get the details of the buffer
"   for which this autocommand got executed. It should return 0 to mean
"   noautoread and 1 to mean autoread the current buffer. It can also return
"   -1 to make its return value ignored and use default autoread mechanism
"   (which could still be overridden by the return value of other functions).
"   The return value of all the functions is ORed to determine the effective
"   autoread value.
"
" void    AddToFCShellPre(String funcName)
" -----------------------
" Remove the given function previously added by calling AddToFCShellPre.
"
" void    RemoveFromFCShellPre(String funcName)
" -----------------------
" Same as AddToFCShellPre except that the function is called after the event
"   is processed, so this is like a fictitious FileChangedShellPost event.
"
" void    AddToFCShell(String funcName)
" -----------------------
" Remove the given function previously added by calling AddToFCShell.
"
" void    RemoveFromFCShell(String funcName)
" -----------------------
" The plugin provides a default autocommand handler which can be installed
"   by calling this function. 
" 
" void    DefFCShellInstall()
" -----------------------
" Uninstall the default autocommand handler that was previously installed
"   using DefFCShellInstall. Calling this function may not actually result in
"   removing the handler, in case there are other callers still dependent on
"   it (which is kept track of by the number of times DefFCShellInstall has
"   been called).
"
" void    DefFCShellUninstall()
" -----------------------
" This function emulates the Vim's default behavior when a |timestamp| change
"   is detected. Register your functions by calling AddToFCShellPre or
"   AddToFCShell and have this function called during the FileChangedShell event
"   (or just install the default handler by calling DefFCShellInstall).
"   From your callbacks, return 1 to mean autoread, 0 to mean noautoread and
"   -1 to mean system default (or ignore). The return value of this method is
"   1 if the file was reloaded and 0 otherwise. The return value of all the
"   functions is ORed to determine the effective autoread value. See my
"   perforce plugin for usage example.
"
" boolean DefFileChangedShell()
" -----------------------
" Execute a substitute command silently and without corrupting the search
"   register.
" Ex:
"   To insert a tab infrontof all lines:
"         call SilentSubstitute('^', '%s//\t/e')
"   To remote all carriage returns at the line ending:
"         call SilentSubstitute("\<CR>$", '%s///e')
"
" void    SilentSubstitute(String pat, String cmd)
" -----------------------
" Delete all lines matching the given pattern silently and without corrupting
"   the search register. The range argument if passed should be a valid prefix
"   for the :global command.
" Ex:
"   To delete all lines that are empty:
"         call SilentDelete('^\s*$')
"   To delete all lines that are empty only in the range 10 to 100:
"         call SilentDelete('10,100', '^\s*$')
"
" void    SilentDelete(String pat)
" void    SilentDelete(String range, String pat)
" -----------------------
" Can return a spacer from 0 to 80 characters width.
"
" String  GetSpacer(int width)
" -----------------------
" -----------------------
" Deprecations:
"   - The g:makeArgumentString and g:makeArgumentList are obsolete and are
"     deprecated, please use MakeArgumentString() and MakeArgumentList()
"     instead.
"   - FindWindowForBuffer() function is now deprecated, as the corresponding
"     Vim bugs are fixed. Use the below expr instead:
"       bufwinnr(FindBufferForName(fileName))
"
" Sample Usages Or Tips:
"   - Add the following command to your vimrc to turn off diff settings.
"       command! DiffOff :call CleanDiffOptions()
"       
"   - Add the following commands to create simple sort commands.
"       command! -nargs=0 -range=% SortByLength <line1>,<line2>call QSort(
"           \ 'CmpByLineLengthNname', 1)
"       command! -nargs=0 -range=% RSortByLength <line1>,<line2>call QSort(
"           \ 'CmpByLineLengthNname', -1)
"       command! -nargs=0 -range=% SortJavaImports <line1>,<line2>call QSort(
"           \ 'CmpJavaImports', 1)
"
"   - You might like the following mappings to adjust spacing:
"       nnoremap <silent> <C-Space> :call ShiftWordInSpace(1)<CR>
"       nnoremap <silent> <C-BS> :call ShiftWordInSpace(-1)<CR>
"       nnoremap <silent> \cw :call CenterWordInSpace()<CR>
"       nnoremap <silent> \va :call AlignWordWithWordInPreviousLine()<CR>
"
"   - The :find command is very useful to search for a file in path, but it
"     doesn't support file completion. Add the following command in your vimrc
"     to add this functionality:
"       command! -nargs=1 -bang -complete=custom,<SID>PathComplete FindInPath
"             \ :find<bang> <args>
"       function! s:PathComplete(ArgLead, CmdLine, CursorPos)
"         return UserFileComplete(a:ArgLead, a:CmdLine, a:CursorPos, 1, &path)
"       endfunction
"
"   - If you are running commands that generate multiple pages of output, you
"     might find it useful to redirect the output to a new buffer. Put the
"     following command in your vimrc:
"       command! -nargs=* -complete=command Redir
"             \ :new | put! =GetVimCmdOutput('<args>') | setl bufhidden=wipe |
"             \ setl nomodified
"
" TODO:
"   - fnamemodify() on Unix doesn't expand to full name if the filename doesn't
"     really exist on the filesystem.
"   - Is setting 'scrolloff' and 'sidescrolloff' to 0 required while moving the
"     cursor?
"
"   - EscapeCommand() didn't work for David Fishburn.
"   - Save/RestoreWindowSettings doesn't work well.
"   - Support specifying arguments (with spaces) enclosed in "" or '' for
"     makeArgumentString. Just combine the arguments that are between "" or ''
"     and strip the quotes off.
"

if exists('loaded_genutils')
  finish
endif
if v:version < 603
  echomsg 'genutils: You need at least Vim 6.3'
  finish
endif

if !exists('loaded_multvals')
  runtime plugin/multvals.vim
endif
if !exists('loaded_multvals') || loaded_multvals < 305
  echomsg 'genutils: You need a newer version of multvals.vim plugin'
  finish
endif
let loaded_genutils = 119

" Make sure line-continuations won't cause any problem. This will be restored
"   at the end
let s:save_cpo = &cpo
set cpo&vim


let g:makeArgumentString = 'exec MakeArgumentString()'
let g:makeArgumentList = 'exec MakeArgumentList()'

let s:makeArgumentString = ''
function! MakeArgumentString(...)
  if s:makeArgumentString == ''
    let s:makeArgumentString = ExtractFuncListing(s:myScriptId.
          \ '_makeArgumentString', 0, 0)
  endif
  if a:0 > 0 && a:1 != ''
    return substitute(s:makeArgumentString, '\<argumentString\>', a:1, 'g')
  else
    return s:makeArgumentString
  endif
endfunction


let s:makeArgumentList = ''
function! MakeArgumentList(...)
  if s:makeArgumentList == ''
    let s:makeArgumentList = ExtractFuncListing(s:myScriptId.
          \ '_makeArgumentList', 0, 0)
  endif
  if a:0 > 0 && a:1 != ''
    let mkArgLst = substitute(s:makeArgumentList, '\<argumentList\>', a:1, 'g')
    if a:0 > 1 && a:2 != ''
      let mkArgLst = substitute(s:makeArgumentList,
            \ '\(\s\+let __argSeparator = \)[^'."\n".']*', "\\1'".a:2."'", '')
    endif
    return mkArgLst
  else
    return s:makeArgumentList
  endif
endfunction

function! ExtractFuncListing(funcName, hLines, tLines)
  let listing = GetVimCmdOutput('func '.a:funcName)
  let listing = substitute(listing,
        \ '^\%(\s\|'."\n".'\)*function '.a:funcName.'([^)]*)'."\n", '', '')
  "let listing = substitute(listing, '\%(\s\|'."\n".'\)*endfunction\%(\s\|'."\n".'\)*$', '', '')
  " Leave the last newline character.
  let listing = substitute(listing, '\%('."\n".'\)\@<=\s*endfunction\s*$', '', '')
  let listing = substitute(listing, '\(\%(^\|'."\n".'\)\s*\)\@<=\d\+',
        \ '', 'g')
  if a:hLines > 0
    let listing = substitute(listing, '^\%([^'."\n".']*'."\n".'\)\{'.
          \ a:hLines.'}', '', '')
  endif
  if a:tLines > 0
    let listing = substitute(listing, '\%([^'."\n".']*'."\n".'\)\{'.
          \ a:tLines.'}$', '', '')
  endif
  return listing
endfunction

function! CreateArgString(argList, sep, ...)
  let sep = (a:0 == 0) ? a:sep : a:1
  call MvIterCreate(a:argList, a:sep, 'CreateArgString', sep)
  let argString = "'"
  while MvIterHasNext('CreateArgString')
    let nextArg = MvIterNext('CreateArgString')
    " FIXME: I think this check is not required. If "'" is the separator, we
    "   don't expect to see them in the elements.
    if a:sep != "'"
      let nextArg = substitute(nextArg, "'", "' . \"'\" . '", 'g')
    endif
    let argString = argString . nextArg . "', '"
  endwhile
  let argString = strpart(argString, 0, strlen(argString) - 3)
  call MvIterDestroy('CreateArgString')
  return argString
endfunction

" {{{
function! s:_makeArgumentString()
  let __argCounter = 1
  let argumentString = ''
  while __argCounter <= a:0
    if type(a:{__argCounter})
      let __nextArg =  "'" .
            \ substitute(a:{__argCounter}, "'", "' . \"'\" . '", "g") . "'"
    else
      let __nextArg = a:{__argCounter}
    endif
    let argumentString = argumentString. __nextArg .
          \ ((__argCounter == a:0) ? '' : ', ')
    let __argCounter = __argCounter + 1
  endwhile
  unlet __argCounter
  if exists('__nextArg')
    unlet __nextArg
  endif
endfunction

function! s:_makeArgumentList()
  let __argCounter = 1
  let __argSeparator = ','
  let argumentList = ''
  while __argCounter <= a:0
    let argumentList = argumentList . a:{__argCounter}
    if __argCounter != a:0
      let argumentList = argumentList . __argSeparator
    endif
    let __argCounter = __argCounter + 1
  endwhile
  unlet __argCounter
  unlet __argSeparator
endfunction
" }}}


function! DebugShowArgs(...)
  let i = 0
  let argString = ''
  while i < a:0
    let argString = argString . a:{i + 1} . ', '
    let i = i + 1
  endwhile
  let argString = strpart(argString, 0, strlen(argString) - 2)
  call input("Args: " . argString)
endfunction

" Window related functions {{{

function! NumberOfWindows()
  let i = 1
  while winbufnr(i) != -1
    let i = i+1
  endwhile
  return i - 1
endfunction

" Find the window number for the buffer passed.
" The fileName argument is treated literally, unlike the bufnr() which treats
"   the argument as a regex pattern.
function! FindWindowForBuffer(bufferName, checkUnlisted)
  return bufwinnr(FindBufferForName(a:bufferName))
endfunction

function! FindBufferForName(fileName)
  " The name could be having extra backslashes to protect certain chars (such
  "   as '#' and '%'), so first expand them.
  return s:FindBufferForName(UnEscape(a:fileName, '#%'))
endfunction

function! s:FindBufferForName(fileName)
  let fileName = Escape(a:fileName, '[?,{')
  let _isf = &isfname
  try
    set isfname-=\
    set isfname-=[
    let i = bufnr('^' . fileName . '$')
  finally
    let &isfname = _isf
  endtry
  return i
endfunction

function! GetBufNameForAu(bufName)
  let bufName = a:bufName
  " Autocommands always require forward-slashes.
  let bufName = substitute(bufName, "\\\\", '/', 'g')
  let bufName = escape(bufName, '*?,{}[ ')
  return bufName
endfunction

function! MoveCursorToWindow(winno)
  if NumberOfWindows() != 1
    execute a:winno . " wincmd w"
  endif
endfunction

function! MoveCurLineToWinLine(n)
  normal! zt
  if a:n == 1
    return
  endif
  let _wrap = &l:wrap
  setl nowrap
  let n = a:n
  if n >= winheight(0)
    let n = winheight(0)
  endif
  let n = n - 1
  execute "normal! " . n . "\<C-Y>"
  let &l:wrap = _wrap
endfunction

function! CloseWindow(win, force)
  let _eventignore = &eventignore
  try
    set eventignore=all
    call MarkActiveWindow()

    let &eventignore = _eventignore
    exec a:win 'wincmd w'
    exec 'close'.(a:force ? '!' : '')
    set eventignore=all

    if a:win < s:curWinnr
      let s:curWinnr = s:curWinnr - 1
    endif
    if a:win < s:prevWinnr
      let s:prevWinnr = s:prevWinnr - 1
    endif
  finally
    call RestoreActiveWindow()
    let &eventignore = _eventignore
  endtry
endfunction

function! MarkActiveWindow()
  let s:curWinnr = winnr()
  " We need to restore the previous-window also at the end.
  silent! wincmd p
  let s:prevWinnr = winnr()
  silent! wincmd p
endfunction

function! RestoreActiveWindow()
  if !exists('s:curWinnr')
    return
  endif

  " Restore the original window.
  if winnr() != s:curWinnr
    exec s:curWinnr'wincmd w'
  endif
  if s:curWinnr != s:prevWinnr
    exec s:prevWinnr'wincmd w'
    wincmd p
  endif
endfunction

function! IsOnlyVerticalWindow()
  let onlyVertWin = 1
  let _eventignore = &eventignore

  try
    "set eventignore+=WinEnter,WinLeave
    set eventignore=all
    call MarkActiveWindow()

    wincmd j
    if winnr() != s:curWinnr
      let onlyVertWin = 0
    else
      wincmd k
      if winnr() != s:curWinnr
	let onlyVertWin = 0
      endif
    endif
  finally
    call RestoreActiveWindow()
    let &eventignore = _eventignore
  endtry
  return onlyVertWin
endfunction

function! IsOnlyHorizontalWindow()
  let onlyHorizWin = 1
  let _eventignore = &eventignore
  try
    set eventignore=all
    call MarkActiveWindow()
    wincmd l
    if winnr() != s:curWinnr
      let onlyHorizWin = 0
    else
      wincmd h
      if winnr() != s:curWinnr
	let onlyHorizWin = 0
      endif
    endif
  finally
    call RestoreActiveWindow()
    let &eventignore = _eventignore
  endtry
  return onlyHorizWin
endfunction

function! MoveCursorToNextInWinStack(dir)
  let newwin = GetNextWinnrInStack(a:dir)
  if newwin != 0
    exec newwin 'wincmd w'
  endif
endfunction

function! GetNextWinnrInStack(dir)
  let newwin = 0
  let _eventignore = &eventignore
  try
    set eventignore=all
    call MarkActiveWindow()
    let newwin = s:GetNextWinnrInStack(a:dir)
  finally
    call RestoreActiveWindow()
    let &eventignore = _eventignore
  endtry
  return newwin
endfunction

function! MoveCursorToLastInWinStack(dir)
  let newwin = GetLastWinnrInStack(a:dir)
  if newwin != 0
    exec newwin 'wincmd w'
  endif
endfunction

function! GetLastWinnrInStack(dir)
  let newwin = 0
  let _eventignore = &eventignore
  try
    set eventignore=all
    call MarkActiveWindow()
    while 1
      let wn = s:GetNextWinnrInStack(a:dir)
      if wn != 0
        let newwin = wn
        exec newwin 'wincmd w'
      else
        break
      endif
    endwhile
  finally
    call RestoreActiveWindow()
    let &eventignore = _eventignore
  endtry
  return newwin
endfunction

" Based on the WinStackMv() function posted by Charles E. Campbell, Jr. on vim
"   mailing list on Jul 14, 2004.
function! s:GetNextWinnrInStack(dir)
  "call Decho("MoveCursorToNextInWinStack(dir<".a:dir.">)")

  let isHorizontalMov = (a:dir ==# 'h' || a:dir ==# 'l') ? 1 : 0

  let orgwin = winnr()
  let orgdim = s:GetWinDim(a:dir, orgwin)

  let _winwidth = &winwidth
  let _winheight = &winheight
  try
    set winwidth=1
    set winheight=1
    exec 'wincmd' a:dir
    let newwin = winnr()
    if orgwin == newwin
      " No more windows in this direction.
      "call Decho("newwin=".newwin." stopped".winheight(newwin)."x".winwidth(newwin))
      return 0
    endif
    if s:GetWinDim(a:dir, newwin) != orgdim
      " Window dimension has changed, indicates a move across window stacks.
      "call Decho("newwin=".newwin." height changed".winheight(newwin)."x".winwidth(newwin))
      return 0
    endif
    " Determine if changing original window height affects current window
    "   height.
    exec orgwin 'wincmd w'
    try
      if orgdim == 1
        exec 'wincmd' (isHorizontalMov ? '_' : '|')
      else
        exec 'wincmd' (isHorizontalMov ? '-' : '<')
      endif
      if s:GetWinDim(a:dir, newwin) != s:GetWinDim(a:dir, orgwin)
        "call Decho("newwin=".newwin." different row".winheight(newwin)."x".winwidth(newwin))
        return 0
      endif
      "call Decho("newwin=".newwin." same row".winheight(newwin)."x".winwidth(newwin))
    finally
      exec (isHorizontalMov ? '' : 'vert') 'resize' orgdim
    endtry

    "call Decho("MoveCursorToNextInWinStack")

    return newwin
  finally
    let &winwidth = _winwidth
    let &winheight = _winheight
  endtry
endfunction

function! s:GetWinDim(dir, win)
  return (a:dir ==# 'h' || a:dir ==# 'l') ? winheight(a:win) : winwidth(a:win)
endfunction

function! OpenWinNoEa(winOpenCmd)
  call s:ExecWinCmdNoEa(a:winOpenCmd)
endfunction

function! CloseWinNoEa(winnr, force)
  call s:ExecWinCmdNoEa(a:winnr.'wincmd w | close'.(a:force?'!':''))
endfunction

function! s:ExecWinCmdNoEa(winCmd)
  let _eventignore = &eventignore
  try
    set eventignore=all
    call MarkActiveWindow()
    windo let w:_winfixheight = &winfixheight
    windo set winfixheight
    call RestoreActiveWindow()

    let &eventignore = _eventignore
    exec a:winCmd
    set eventignore=all

    call MarkActiveWindow()
    silent! windo let &winfixheight = w:_winfixheight
    silent! windo unlet w:_winfixheight
    call RestoreActiveWindow()
  finally
    let &eventignore = _eventignore
  endtry
endfunction

" Window related functions }}}

function! SetupScratchBuffer()
  setlocal nobuflisted
  setlocal noswapfile
  setlocal buftype=nofile
  " Just in case, this will make sure we are always hidden.
  setlocal bufhidden=delete
  setlocal nolist
  setlocal nonumber
  setlocal foldcolumn=0 nofoldenable
  setlocal noreadonly
endfunction

function! CleanDiffOptions()
  setlocal nodiff
  setlocal noscrollbind
  setlocal scrollopt-=hor
  setlocal wrap
  setlocal foldmethod=manual
  setlocal foldcolumn=0
  normal zE
endfunction

function! ArrayVarExists(varName, index)
  try
    exec "let test = " . a:varName . "{a:index}"
  catch /^Vim\%((\a\+)\)\=:E121/
    return 0
  endtry
  return 1
endfunction

function! Escape(str, chars)
  return substitute(a:str, '\\\@<!\(\%(\\\\\)*\)\([' . a:chars .']\)', '\1\\\2',
        \ 'g')
endfunction

function! UnEscape(str, chars)
  return substitute(a:str, '\\\@<!\(\\\\\)*\\\([' . a:chars . ']\)',
        \ '\1\2', 'g')
endfunction

function! DeEscape(str)
  let str = a:str
  let str = substitute(str, '\\\(\\\|[^\\]\)', '\1', 'g')
  return str
endfunction

" - For windoze+native, use double-quotes to sorround the arguments and for
"   embedded double-quotes, just double them.
" - For windoze+sh, use single-quotes to sorround the aruments and for embedded
"   single-quotes, just replace them with '""'""' (if 'shq' or 'sxq' is a
"   double-quote) and just '"'"' otherwise. Embedded double-quotes also need
"   to be doubled.
" - For Unix+sh, use single-quotes to sorround the arguments and for embedded
"   single-quotes, just replace them with '"'"'. 
function! EscapeCommand(cmd, args, pipe)
  let fullCmd = a:args
  " I am only worried about passing arguments with spaces as they are to the
  "   external commands, I currently don't care about back-slashes
  "   (backslashes are normally expected only on windows when 'shellslash'
  "   option is set, but even then the 'shell' is expected to take care of
  "   them.). However, for cygwin bash, there is a loss of one level
  "   of the back-slashes somewhere in the chain of execution (most probably
  "   between CreateProcess() and cygwin?), so we need to double them.
  let shellEnvType = GetShellEnvType()
  if shellEnvType ==# g:ST_WIN_CMD
    let quoteChar = '"'
    " Escape the existing double-quotes (by doubling them).
    let fullCmd = substitute(fullCmd, '"', '""', 'g')
  else
    let quoteChar = "'"
    if shellEnvType ==# g:ST_WIN_SH
      " Escape the existing double-quotes (by doubling them).
      let fullCmd = substitute(fullCmd, '"', '""', 'g')
    endif
    " Take care of existing single-quotes (by exposing them, as you can't have
    "   single-quotes inside a single-quoted string).
    if &shellquote == '"' || &shellxquote == '"'
      let squoteRepl = "'\"\"'\"\"'"
    else
      let squoteRepl = "'\"'\"'"
    endif
    let fullCmd = substitute(fullCmd, "'", squoteRepl, 'g')
  endif

  " Now sorround the arguments with quotes, considering the protected
  "   spaces.
  let fullCmd = substitute(fullCmd, '\(\%([^ ]\|\\\@<=\%(\\\\\)* \)\+\)',
        \ quoteChar.'\1'.quoteChar, 'g')
  " We delay adding pipe part so that we can avoid the above processing.
  if a:pipe !~# '^\s*$'
    let fullCmd = fullCmd . ' ' . a:pipe
  endif
  let fullCmd = UnEscape(fullCmd, ' ') " Unescape just the spaces.
  if a:cmd !~# '^\s*$'
    let fullCmd = a:cmd . ' ' . fullCmd
  endif
  if shellEnvType ==# g:ST_WIN_SH && &shell =~# '\<bash\>'
    let fullCmd = substitute(fullCmd, '\\', '\\\\', 'g')
  endif
  return fullCmd
endfunction

let g:ST_WIN_CMD = 0 | let g:ST_WIN_SH = 1 | let g:ST_UNIX = 2
function! GetShellEnvType()
  " When 'shellslash' option is available, then the platform must be one of
  "     those that support '\' as a pathsep.
  if exists('+shellslash')
    if stridx(&shell, 'cmd.exe') != -1 ||
          \ stridx(&shell, 'command.com') != -1
      return g:ST_WIN_CMD
    else
      return g:ST_WIN_SH
    endif
  else
    return g:ST_UNIX
  endif
endfunction

function! ExpandStr(str)
  let str = substitute(a:str, '"', '\\"', 'g')
  exec "let str = \"" . str . "\"" 
  return str
endfunction

function! QuoteStr(str)
  return "'".substitute(a:str, "'", "'.\"'\".'", 'g')."'"
endfunction

function! GetPreviewWinnr()
  let _eventignore = &eventignore
  let curWinNr = winnr()
  let winnr = -1
  try
    set eventignore=all
    exec "wincmd P"
    let winnr = winnr()
  catch /^Vim\%((\a\+)\)\=:E441/
    " Ignore, winnr is already set to -1.
  finally
    if winnr() != curWinNr
      exec curWinNr.'wincmd w'
    endif
    let &eventignore = _eventignore
  endtry
  return winnr
endfunction

" Save/Restore window settings {{{
function! SaveWindowSettings()
  call SaveWindowSettings2('SaveWindowSettings', 1)
endfunction

function! RestoreWindowSettings()
  call RestoreWindowSettings2('SaveWindowSettings')
endfunction


function! ResetWindowSettings()
  call ResetWindowSettings2('SaveWindowSettings')
endfunction

function! SaveWindowSettings2(sid, overwrite)
  if ArrayVarExists("s:winSettings", a:sid) && ! a:overwrite
    return
  endif

  let s:winSettings{a:sid} = ""
  let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",",
          \ NumberOfWindows())
  let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",", &lines)
  let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",", &columns)
  let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",", winnr())
  let i = 1
  while winbufnr(i) != -1
    let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",",
            \ winheight(i))
    let s:winSettings{a:sid} = MvAddElement(s:winSettings{a:sid}, ",",
            \ winwidth(i))
    let i = i + 1
  endwhile
  "let g:savedWindowSettings = s:winSettings{a:sid} " Debug.
endfunction

function! RestoreWindowSettings2(sid)
  " Calling twice fixes most of the resizing issues. This seems to be how the
  " :mksession with "winsize" in 'sesionoptions' seems to work.
  call s:RestoreWindowSettings2(a:sid)
  call s:RestoreWindowSettings2(a:sid)
endfunction

function! s:RestoreWindowSettings2(sid)
  "if ! exists("s:winSettings" . a:sid)
  if ! ArrayVarExists("s:winSettings", a:sid)
    return
  endif

  call MvIterCreate(s:winSettings{a:sid}, ",", "savedWindowSettings")
  let nWindows = MvIterNext("savedWindowSettings")
  if nWindows != NumberOfWindows()
    unlet s:winSettings{a:sid}
    call MvIterDestroy("savedWindowSettings")
    return
  endif
  let orgLines = MvIterNext("savedWindowSettings")
  let orgCols = MvIterNext("savedWindowSettings")
  let activeWindow = MvIterNext("savedWindowSettings")
  let mainWinSizeSame = (orgLines == &lines && orgCols == &columns)

  let winNo = 1
  while MvIterHasNext("savedWindowSettings")
    let height = MvIterNext("savedWindowSettings")
    let width = MvIterNext("savedWindowSettings")
    let height = (mainWinSizeSame ? height :
          \ ((&lines * height + (orgLines / 2)) / orgLines))
    let width = (mainWinSizeSame ? width :
          \ ((&columns * width + (orgCols / 2)) / orgCols))
    if winheight(winnr()) != height
      exec winNo'resize' height
    endif
    if winwidth(winnr()) != width
      exec 'vert' winNo 'resize' width
    endif
    let winNo = winNo + 1
  endwhile
  
  " Restore the current window.
  call MoveCursorToWindow(activeWindow)
  "unlet g:savedWindowSettings " Debug.
  call MvIterDestroy("savedWindowSettings")
endfunction


function! ResetWindowSettings2(sid)
  if ArrayVarExists("s:winSettings", a:sid)
    unlet s:winSettings{a:sid}
  endif
endfunction

" Save/Restore window settings }}}

" Save/Restore selection {{{

function! SaveVisualSelection(sid)
  let curmode = mode()
  if curmode == 'n'
    normal! gv
  endif
  let s:vismode{a:sid} = mode()
  let s:firstline{a:sid} = line("'<")
  let s:lastline{a:sid} = line("'>")
  let s:firstcol{a:sid} = col("'<")
  let s:lastcol{a:sid} = col("'>")
  if curmode !=# s:vismode{a:sid}
    exec "normal \<Esc>"
  endif
endfunction

function! RestoreVisualSelection(sid)
  if mode() !=# 'n'
    exec "normal \<Esc>"
  endif
  if exists('s:vismode{sid}')
    exec 'normal' s:firstline{a:sid}.'gg'.s:firstcol{a:sid}.'|'.
          \ s:vismode{a:sid}.(s:lastline{a:sid}-s:firstline{a:sid}).'j'.
          \ (s:lastcol{a:sid}-s:firstcol{a:sid}).'l'
  endif
endfunction
" Save/Restore selection }}}

function! CleanupFileName(fileName)
  let fileName = a:fileName

  " Expand relative paths and paths containing relative components (takes care
  " of ~ also).
  if ! PathIsAbsolute(fileName)
    let fileName = fnamemodify(fileName, ':p')
  endif

  " I think we can have UNC paths on UNIX, if samba is installed.
  if (match(fileName, '^//') == 0) || (OnMS() && match(fileName, '^\\\\') == 0)
    let uncPath = 1
  else
    let uncPath = 0
  endif

  " Remove multiple path separators.
  if has('win32')
    let fileName=substitute(fileName, '\\', '/', 'g')
  elseif OnMS()
    let fileName=substitute(fileName, '\\\{2,}', '\', 'g')
  endif
  let fileName=substitute(fileName, '/\{2,}', '/', 'g')

  " Remove ending extra path separators.
  let fileName=substitute(fileName, '/$', '', '')
  let fileName=substitute(fileName, '\$', '', '')

  " If it was an UNC path, add back an extra slash.
  if uncPath
    let fileName = '/'.fileName
  endif

  if OnMS()
    let fileName=substitute(fileName, '^[A-Z]:', '\L&', '')

    " Add drive letter if missing (just in case).
    if !uncPath && match(fileName, '^/') == 0
      let curDrive = substitute(getcwd(), '^\([a-zA-Z]:\).*$', '\L\1', '')
      let fileName = curDrive . fileName
    endif
  endif
  return fileName
endfunction
"echo CleanupFileName('\\a///b/c\')
"echo CleanupFileName('C:\a/b/c\d')
"echo CleanupFileName('a/b/c\d')
"echo CleanupFileName('~/a/b/c\d')
"echo CleanupFileName('~/a/b/../c\d')

function! OnMS()
  return has('win32') || has('dos32') || has('win16') || has('dos16') ||
       \ has('win95')
endfunction

function! PathIsAbsolute(path)
  let absolute=0
  if has('unix') || OnMS()
    if match(a:path, '^/') == 0
      let absolute=1
    endif
  endif
  if (! absolute) && OnMS()
    if match(a:path, "^\\") == 0
      let absolute=1
    endif
  endif
  if (! absolute) && OnMS()
    if match(a:path, "^[A-Za-z]:") == 0
      let absolute=1
    endif
  endif
  return absolute
endfunction

function! PathIsFileNameOnly(path)
  return (match(a:path, "\\") < 0) && (match(a:path, "/") < 0)
endfunction


" Copy this method into your script and rename it to find the script id of the
"   current script.
function! s:MyScriptId()
  map <SID>xx <SID>xx
  let s:sid = maparg("<SID>xx")
  unmap <SID>xx
  return substitute(s:sid, "xx$", "", "")
endfunction
let s:myScriptId = s:MyScriptId()
delfunc s:MyScriptId " Not required any more.


"" --- START save/restore position. {{{

function! SaveSoftPosition(id)
  let b:sp_startline_{a:id} = getline(".")
  call SaveHardPosition(a:id)
endfunction

function! RestoreSoftPosition(id)
  0
  call RestoreHardPosition(a:id)
  let stLine = b:sp_startline_{a:id}
  if getline('.') !=# stLine
    if ! search('\V\^'.escape(stLine, "\\").'\$', 'W') 
      call search('\V\^'.escape(stLine, "\\").'\$', 'bW')
    endif
  endif
  call MoveCurLineToWinLine(b:sp_winline_{a:id})
endfunction

function! ResetSoftPosition(id)
  unlet b:sp_startline_{a:id}
endfunction

" A synonym for SaveSoftPosition.
function! SaveHardPositionWithContext(id)
  call SaveSoftPosition(a:id)
endfunction

" A synonym for RestoreSoftPosition.
function! RestoreHardPositionWithContext(id)
  call RestoreSoftPosition(a:id)
endfunction

" A synonym for ResetSoftPosition.
function! ResetHardPositionWithContext(id)
  call ResetSoftPosition(a:id)
endfunction

function! SaveHardPosition(id)
  let b:sp_col_{a:id} = virtcol(".")
  let b:sp_lin_{a:id} = line(".")
  let b:sp_winline_{a:id} = winline()
endfunction

function! RestoreHardPosition(id)
  " This doesn't take virtual column.
  "call cursor(b:sp_lin_{a:id}, b:sp_col_{a:id})
  " Vim7 generates E16 if line number is invalid.
  " TODO: Why is this leaving cursor on the last-but-one line when the
  " condition meets?
  execute ((line('$') < b:sp_lin_{a:id}) ? line('$') :
        \ b:sp_lin_{a:id})
  "execute b:sp_lin_{a:scriptid}
  execute ((line('$') < b:sp_lin_{a:id}) ? line('$') :
        \ b:sp_lin_{a:id})
  "execute b:sp_lin_{a:id}
  execute "normal!" b:sp_col_{a:id} . "|"
  call MoveCurLineToWinLine(b:sp_winline_{a:id})
endfunction

function! ResetHardPosition(id)
  unlet b:sp_col_{a:id}
  unlet b:sp_lin_{a:id}
  unlet b:sp_winline_{a:id}
endfunction

function! GetLinePosition(id)
  return b:sp_lin_{a:id}
endfunction

function! GetColPosition(id)
  return b:sp_col_{a:id}
endfunction

function! IsPositionSet(id)
  return exists('b:sp_col_' . a:id)
endfunction

"" --- END save/restore position. }}}



""
"" --- START: Notify window close -- {{{
""

function! AddNotifyWindowClose(windowTitle, functionName)
  let bufName = a:windowTitle

  " Make sure there is only one entry per window title.
  if exists("s:notifyWindowTitles") && s:notifyWindowTitles != ""
    call RemoveNotifyWindowClose(bufName)
  endif

  if !exists("s:notifyWindowTitles")
    " Both separated by :.
    let s:notifyWindowTitles = ""
    let s:notifyWindowFunctions = ""
  endif

  let s:notifyWindowTitles = MvAddElement(s:notifyWindowTitles, ";", bufName)
  let s:notifyWindowFunctions = MvAddElement(s:notifyWindowFunctions, ";",
          \ a:functionName)

  "let g:notifyWindowTitles = s:notifyWindowTitles " Debug.
  "let g:notifyWindowFunctions = s:notifyWindowFunctions " Debug.

  " Start listening to events.
  aug NotifyWindowClose
    au!
    au WinEnter * :call CheckWindowClose()
    au BufEnter * :call CheckWindowClose()
  aug END
endfunction

function! RemoveNotifyWindowClose(windowTitle)
  let bufName = a:windowTitle

  if !exists("s:notifyWindowTitles")
    return
  endif

  if MvContainsElement(s:notifyWindowTitles, ";", bufName)
    let index = MvIndexOfElement(s:notifyWindowTitles, ";", bufName)
    let s:notifyWindowTitles = MvRemoveElementAt(s:notifyWindowTitles, ";",
            \ index)
    let s:notifyWindowFunctions = MvRemoveElementAt(s:notifyWindowFunctions,
            \ ";", index)

    if s:notifyWindowTitles == ""
      unlet s:notifyWindowTitles
      unlet s:notifyWindowFunctions
      "unlet g:notifyWindowTitles " Debug.
      "unlet g:notifyWindowFunctions " Debug.
  
      aug NotifyWindowClose
        au!
      aug END
    endif
  endif
endfunction

function! CheckWindowClose()
  if !exists('s:notifyWindowTitles') || s:notifyWindowTitles == ''
    return
  endif

  " Now iterate over all the registered window titles and see which one's are
  "   closed.
  " Take a copy and iterate over them, such that we can freely modify the main
  "   arrays as needed (and the caller can also concurrently modify them)
  let windowFuncsCopy = s:notifyWindowFunctions
  call MvIterCreate(s:notifyWindowTitles, ';', 'NotifyWindowClose')
  let i = 0 " To track the element index.
  while MvIterHasNext('NotifyWindowClose')
    let nextWin = MvIterNext('NotifyWindowClose')
    if bufwinnr(s:FindBufferForName(nextWin)) == -1
      let funcName = MvElementAt(windowFuncsCopy, ';', i)
      let cmd = 'call ' . funcName . "('" . nextWin . "')"
      " Remove these entries as these are going to be processed. This also
      "   allows the caller to add them back if needed.
      call RemoveNotifyWindowClose(nextWin)
      "call input("cmd: " . cmd)
      exec cmd
    endif
    let i = i + 1
  endwhile
  call MvIterDestroy('NotifyWindowClose')
endfunction

"function! NotifyWindowCloseF(title)
"  call input(a:title . " closed")
"endfunction
"
"function! RunNotifyWindowCloseTest()
"  call input("Creating three windows, 'ABC', 'XYZ' and 'b':")
"  split ABC
"  split X Y Z
"  split b
"  redraw
"  call AddNotifyWindowClose("ABC", "NotifyWindowCloseF")
"  call AddNotifyWindowClose("X Y Z", "NotifyWindowCloseF")
"  call input("notifyWindowTitles: " . s:notifyWindowTitles)
"  call input("notifyWindowFunctions: " . s:notifyWindowFunctions)
"  au NotifyWindowClose WinEnter
"  call input("Added notifications for 'ABC' and 'XYZ'\n".
"       \ "Now closing the windows, you should see ONLY two notifications:")
"  quit
"  quit
"  quit
"endfunction

""
"" --- END: Notify window close -- }}}
""

" TODO: For large ranges, the cmd can become too big, so make it one cmd per
"       line.
function! ShowLinesWithSyntax() range
  " This makes sure we start (subsequent) echo's on the first line in the
  " command-line area.
  "
  echo ''

  let cmd        = ''
  let prev_group = ' x '     " Something that won't match any syntax group.

  let show_line = a:firstline
  let isMultiLine = ((a:lastline - a:firstline) > 1)
  while show_line <= a:lastline
    let cmd = ''
    let length = strlen(getline(show_line))
    let column = 1

    while column <= length
      let group = synIDattr(synID(show_line, column, 1), 'name')
      if group != prev_group
        if cmd != ''
          let cmd = cmd . "'|"
        endif
        let cmd = cmd . 'echohl ' . (group == '' ? 'NONE' : group) . "|echon '"
        let prev_group = group
      endif
      let char = strpart(getline(show_line), column - 1, 1)
      if char == "'"
        let char = "'."'".'"
      endif
      let cmd = cmd . char
      let column = column + 1
    endwhile

    try
      exec cmd."\n'"
    catch
      echo ''
    endtry
    let show_line = show_line + 1
  endwhile
  echohl NONE
endfunction


function! AlignWordWithWordInPreviousLine()
  "First go to the first col in the word.
  if getline('.')[col('.') - 1] =~ '\s'
    normal! w
  else
    normal! "_yiw
  endif
  let orgVcol = virtcol('.')
  let prevLnum = prevnonblank(line('.') - 1)
  if prevLnum == -1
    return
  endif
  let prevLine = getline(prevLnum)

  " First get the column to align with.
  if prevLine[orgVcol - 1] =~ '\s'
    " column starts from 1 where as index starts from 0.
    let nonSpaceStrInd = orgVcol " column starts from 1 where as index starts from 0.
    while prevLine[nonSpaceStrInd] =~ '\s'
      let nonSpaceStrInd = nonSpaceStrInd + 1
    endwhile
  else
    if strlen(prevLine) < orgVcol
      let nonSpaceStrInd = strlen(prevLine) - 1
    else
      let nonSpaceStrInd = orgVcol - 1
    endif

    while prevLine[nonSpaceStrInd - 1] !~ '\s' && nonSpaceStrInd > 0
      let nonSpaceStrInd = nonSpaceStrInd - 1
    endwhile
  endif
  let newVcol = nonSpaceStrInd + 1 " Convert to column number.

  if orgVcol > newVcol " We need to reduce the spacing.
    let sub = strpart(getline('.'), newVcol - 1, (orgVcol - newVcol))
    if sub =~ '^\s\+$'
      " Remove the excess space.
      exec 'normal! ' . newVcol . '|'
      exec 'normal! ' . (orgVcol - newVcol) . 'x'
    endif
  elseif orgVcol < newVcol " We need to insert spacing.
    exec 'normal! ' . orgVcol . '|'
    exec 'normal! ' . (newVcol - orgVcol) . 'i '
  endif
endfunction

function! ShiftWordInSpace(dir)
  if a:dir == 1 " forward.
    " If currently on <Space>...
    if getline(".")[col(".") - 1] == " "
      let move1 = 'wf '
    else
      " If next col is a 
      "if getline(".")[col(".") + 1]
      let move1 = 'f '
    endif
    let removeCommand = "x"
    let pasteCommand = "bi "
    let move2 = 'w'
    let offset = 0
  else " backward.
    " If currently on <Space>...
    if getline(".")[col(".") - 1] == " "
      let move1 = 'w'
    else
      let move1 = '"_yiW'
    endif
    let removeCommand = "hx"
    let pasteCommand = 'h"_yiwEa '
    let move2 = 'b'
    let offset = -3
  endif

  let savedCol = col(".")
  exec "normal!" move1
  let curCol = col(".")
  let possible = 0
  " Check if there is a space at the end.
  if col("$") == (curCol + 1) " Works only for forward case, as expected.
    let possible = 1
  elseif getline(".")[curCol + offset] == " "
    " Remove the space from here.
    exec "normal!" removeCommand
    let possible = 1
  endif

  " Move back into the word.
  "exec "normal!" savedCol . "|"
  if possible == 1
    exec "normal!" pasteCommand
    exec "normal!" move2
  else
    " Move to the original location.
    exec "normal!" savedCol . "|"
  endif
endfunction


function! CenterWordInSpace()
  let line = getline('.')
  let orgCol = col('.')
  " If currently on <Space>...
  if line[orgCol - 1] == " "
    let matchExpr = ' *\%'. orgCol . 'c *\w\+ \+'
  else
    let matchExpr = ' \+\(\w*\%' . orgCol . 'c\w*\) \+'
  endif
  let matchInd = match(line, matchExpr)
  if matchInd == -1
    return
  endif
  let matchStr = matchstr(line,  matchExpr)
  let nSpaces = strlen(substitute(matchStr, '[^ ]', '', 'g'))
  let word = substitute(matchStr, ' ', '', 'g')
  let middle = nSpaces / 2
  let left = nSpaces - middle
  let newStr = ''
  while middle > 0
    let newStr = newStr . ' '
    let middle = middle - 1
  endwhile
  let newStr = newStr . word
  while left > 0
    let newStr = newStr . ' '
    let left = left - 1
  endwhile

  let newLine = strpart(line, 0, matchInd)
  let newLine = newLine . newStr
  let newLine = newLine . strpart (line, matchInd + strlen(matchStr))
  silent! keepjumps call setline(line('.'), newLine)
endfunction

function! MapAppendCascaded(lhs, rhs, mapMode)

  " Determine the map mode from the map command.
  let mapChar = strpart(a:mapMode, 0, 1)

  " Check if this is already mapped.
  let oldrhs = maparg(a:lhs, mapChar)
  if oldrhs != ""
    let self = oldrhs
  else
    let self = a:lhs
  endif
  "echomsg a:mapMode . "oremap" . " " . a:lhs . " " . self . a:rhs
  exec a:mapMode . "oremap" a:lhs self . a:rhs
endfunction

" smartSlash simply says whether to depend on shellslash and ArgLead for
"   determining path separator. If it shouldn't depend, it will always assume
"   that the required pathsep is forward-slash.
function! UserFileComplete(ArgLead, CmdLine, CursorPos, smartSlash, searchPath)
  let glob = ''
  let opathsep = "\\"
  let npathsep = '/'
  if exists('+shellslash') && ! &shellslash && a:smartSlash &&
        \ stridx(a:ArgLead, "\\") != -1
    let opathsep = '/'
    let npathsep = "\\"
  endif
  if a:searchPath !=# ''
    call MvIterCreate(a:searchPath, MvCrUnProtectedCharsPattern(','),
          \ 'UserFileComplete', ',')
    while MvIterHasNext('UserFileComplete')
      let nextPath = CleanupFileName(MvIterNext('UserFileComplete'))
      let matches = glob(nextPath.'/'.a:ArgLead.'*')
      if matches !~# '^\_s*$'
        let matches = s:FixPathSep(matches, opathsep, npathsep)
        let nextPath = substitute(nextPath, opathsep, npathsep, 'g')
        let matches = substitute(matches, '\V'.escape(nextPath.npathsep, "\\"),
              \ '', 'g')
        let glob = glob . matches . "\n"
      endif
    endwhile
    call MvIterDestroy('UserFileComplete')
  else
    let glob = s:FixPathSep(glob(a:ArgLead.'*'), opathsep, npathsep)
  endif
  return glob
endfunction

command! -complete=file -nargs=* GUDebugEcho :echo <q-args>
function! UserFileExpand(fileArgs)
  return substitute(GetVimCmdOutput(
        \ 'GUDebugEcho ' . a:fileArgs), '^\_s\+\|\_s\+$', '', 'g')
endfunction

function! s:FixPathSep(matches, opathsep, npathsep)
  let matches = a:matches
  let matches = substitute(matches, a:opathsep, a:npathsep, 'g')
  let matches = substitute(matches, "\\([^\n]\\+\\)", '\=submatch(1).'.
        \ '(isdirectory(submatch(1)) ? a:npathsep : "")', 'g')
  return matches
endfunction

function! GetVimCmdOutput(cmd)
  let v:errmsg = ''
  let output = ''
  let _z = @z
  let _shortmess = &shortmess
  try
    set shortmess=
    redir @z
    silent exec a:cmd
  catch /.*/
    let v:errmsg = substitute(v:exception, '^[^:]\+:', '', '')
  finally
    redir END
    let &shortmess = _shortmess
    if v:errmsg == ''
      let output = @z
    endif
    let @z = _z
  endtry
  return output
endfunction

function! OptClearBuffer()
  " Go as far as possible in the undo history to conserve Vim resources.
  let _modifiable = &l:modifiable
  let _undolevels = &undolevels
  try
    setl modifiable
    set undolevels=-1
    silent! keepjumps 0,$delete _
  finally
    let &undolevels = _undolevels
    let &l:modifiable = _modifiable
  endtry
endfunction


"" START: Sorting support. {{{
""

"
" Comapare functions.
"

function! s:CmpByLineLengthNname(line1, line2, direction)
  return CmpByLineLengthNname(a:line1, a:line2, a:direction)
endfunction

function! CmpByLineLengthNname(line1, line2, direction)
  let cmp = CmpByLength(a:line1, a:line2, a:direction)
  if cmp == 0
    let cmp = CmpByString(a:line1, a:line2, a:direction)
  endif
  return cmp
endfunction

function! s:CmpByLength(line1, line2, direction)
  return CmpByLength(a:line1, a:line2, a:direction)
endfunction


function! CmpByLength(line1, line2, direction)
  let len1 = strlen(a:line1)
  let len2 = strlen(a:line2)
  return a:direction * (len1 - len2)
endfunction

function! s:CmpJavaImports(line1, line2, direction)
  return CmpJavaImports(a:line1, a:line2, a:direction)
endfunction

" Compare first by name and then by length.
" Useful for sorting Java imports.
function! CmpJavaImports(line1, line2, direction)
  " FIXME: Simplify this.
  if stridx(a:line1, '.') == -1
    let pkg1 = ''
    let cls1 = substitute(a:line1, '.* \(^[ ]\+\)', '\1', '')
  else
    let pkg1 = substitute(a:line1, '.*import\s\+\(.*\)\.[^. ;]\+.*$', '\1', '')
    let cls1 = substitute(a:line1, '^.*\.\([^. ;]\+\).*$', '\1', '')
  endif
  if stridx(a:line2, '.') == -1
    let pkg2 = ''
    let cls2 = substitute(a:line2, '.* \(^[ ]\+\)', '\1', '')
  else
    let pkg2 = substitute(a:line2, '.*import\s\+\(.*\)\.[^. ;]\+.*$', '\1', '')
    let cls2 = substitute(a:line2, '^.*\.\([^. ;]\+\).*$', '\1', '')
  endif

  let cmp = CmpByString(pkg1, pkg2, a:direction)
  if cmp == 0
    let cmp = CmpByLength(cls1, cls2, a:direction)
  endif
  return cmp
endfunction

function! s:CmpByString(line1, line2, direction)
  return CmpByString(a:line1, a:line2, a:direction)
endfunction

function! CmpByString(line1, line2, direction)
  if a:line1 < a:line2
    return -a:direction
  elseif a:line1 > a:line2
    return a:direction
  else
    return 0
  endif
endfunction

function! CmpByStringIgnoreCase(line1, line2, direction)
  if a:line1 <? a:line2
    return -a:direction
  elseif a:line1 >? a:line2
    return a:direction
  else
    return 0
  endif
endfunction

function! s:CmpByNumber(line1, line2, direction)
  return CmpByNumber(a:line1, a:line2, a:direction)
endfunction

function! CmpByNumber(line1, line2, direction)
  let num1 = a:line1 + 0
  let num2 = a:line2 + 0

  if num1 < num2
    return -a:direction
  elseif num1 > num2
    return a:direction
  else
    return 0
  endif
endfunction

function! QSort(cmp, direction) range
  call s:QSortR(a:firstline, a:lastline, a:cmp, a:direction,
        \ 's:BufLineAccessor', 's:BufLineSwapper', '')
endfunction

function! QSort2(start, end, cmp, direction, accessor, swapper, context)
  call s:QSortR(a:start, a:end, a:cmp, a:direction, a:accessor, a:swapper,
        \ a:context)
endfunction

" The default swapper that swaps lines in the current buffer.
function! s:BufLineSwapper(line1, line2, context)
  let str2 = getline(a:line1)
  silent! keepjumps call setline(a:line1, getline(a:line2))
  silent! keepjumps call setline(a:line2, str2)
endfunction

" The default accessor that returns lines from the current buffer.
function! s:BufLineAccessor(line, context)
  return getline(a:line)
endfunction

" The default mover that moves lines from one place to another in the current
" buffer.
function! s:BufLineMover(from, to, context)
  let line = getline(a:from)
  exec a:from.'d_'
  call append(a:to, line)
endfunction

"
" Sort lines.  QSortR() is called recursively.
"
function! s:QSortR(start, end, cmp, direction, accessor, swapper, context)
  if a:end > a:start
    let low = a:start
    let high = a:end

    " Arbitrarily establish partition element at the midpoint of the data.
    let midStr = {a:accessor}(((a:start + a:end) / 2), a:context)

    " Loop through the data until indices cross.
    while low <= high

      " Find the first element that is greater than or equal to the partition
      "   element starting from the left Index.
      while low < a:end
        let result = {a:cmp}({a:accessor}(low, a:context), midStr, a:direction)
        if result < 0
          let low = low + 1
        else
          break
        endif
      endwhile

      " Find an element that is smaller than or equal to the partition element
      "   starting from the right Index.
      while high > a:start
        let result = {a:cmp}({a:accessor}(high, a:context), midStr, a:direction)
        if result > 0
          let high = high - 1
        else
          break
        endif
      endwhile

      " If the indexes have not crossed, swap.
      if low <= high
        " Swap lines low and high.
        call {a:swapper}(high, low, a:context)
        let low = low + 1
        let high = high - 1
      endif
    endwhile

    " If the right index has not reached the left side of data must now sort
    "   the left partition.
    if a:start < high
      call s:QSortR(a:start, high, a:cmp, a:direction, a:accessor, a:swapper,
            \ a:context)
    endif

    " If the left index has not reached the right side of data must now sort
    "   the right partition.
    if low < a:end
      call s:QSortR(low, a:end, a:cmp, a:direction, a:accessor, a:swapper,
            \ a:context)
    endif
  endif
endfunction

function! BinSearchForInsert(start, end, line, cmp, direction)
  return BinSearchForInsert2(a:start, a:end, a:line, a:cmp, a:direction,
        \ 's:BufLineAccessor', '')
endfunction

function! BinSearchForInsert2(start, end, line, cmp, direction, accessor,
      \ context)
  let start = a:start - 1
  let end = a:end
  while start < end
    let middle = (start + end + 1) / 2
    let result = {a:cmp}({a:accessor}(middle, a:context), a:line, a:direction)
    if result < 0
      let start = middle
    else
      let end = middle - 1
    endif
  endwhile
  return start
endfunction

function! BinInsertSort(cmp, direction) range
  call BinInsertSort2(a:firstline, a:lastline, a:cmp, a:direction,
        \ 's:BufLineAccessor', 's:BufLineMover', '')
endfunction

function! BinInsertSort2(start, end, cmp, direction, accessor, mover, context)
  let i = a:start + 1
  while i <= a:end
    let low = s:BinSearchToAppend2(a:start, i, {a:accessor}(i, a:context),
          \ a:cmp, a:direction, a:accessor, a:context)
    " Move the object.
    if low < i
      call {a:mover}(i, low - 1, a:context)
    endif
    let i = i + 1
  endwhile
endfunction

function! s:BinSearchToAppend(start, end, line, cmp, direction)
  return s:BinSearchToAppend2(a:start, a:end, a:line, a:cmp, a:direction,
        \ 's:BufLineAccessor', '')
endfunction

function! s:BinSearchToAppend2(start, end, line, cmp, direction, accessor,
      \ context)
  let low = a:start
  let high = a:end
  while low < high
    let mid = (low + high) / 2
    let diff = {a:cmp}({a:accessor}(mid, a:context), a:line, a:direction)
    if diff > 0
      let high = mid
    else
      let low = mid + 1
      if diff == 0
        break
      endif
    endif
  endwhile
  return low
endfunction

""" END: Sorting support. }}}


" Eats character if it matches the given pattern.
"
" Originally,
" From: Benji Fisher <fisherbb@bc.edu>
" Date: Mon, 25 Mar 2002 15:05:14 -0500
"
" Based on Bram's idea of eating a character while type <Space> to expand an
"   abbreviation. This solves the problem with abbreviations, where we are
"   left with an extra space after the expansion.
" Ex:
"   inoreabbr \stdout\ System.out.println("");<Left><Left><Left><C-R>=EatChar('\s')<CR>
function! EatChar(pat)
   let c = nr2char(getchar())
   "call input('Pattern: '.a:pat.' '.
   "      \ ((c =~ a:pat) ? 'Returning empty' : 'Returning: '.char2nr(c)))
   return (c =~ a:pat) ? '' : c
endfun


" Can return a spacer from 0 to 80 characters width.
let s:spacer= "                                                               ".
      \ "                 "
function! GetSpacer(width)
  return strpart(s:spacer, 0, a:width)
endfunction

function! SilentSubstitute(pat, cmd)
  let _search = @/
  try
    let @/ = a:pat
    keepjumps silent! exec a:cmd
  finally
    let @/ = _search
  endtry
endfunction

function! SilentDelete(arg1, ...)
  " For backwards compatibility.
  if a:0
    let range = a:arg1
    let pat = a:1
  else
    let range = ''
    let pat = a:arg1
  endif
  let _search = @/
  try
    let @/ = pat
    keepjumps silent! exec range'g//d _'
  finally
    let @/ = _search
  endtry
endfunction

" START: Roman2Decimal {{{
let s:I = 1
let s:V = 5
let s:X = 10
let s:L = 50
let s:C = 100
let s:D = 500
let s:M = 1000

function! s:Char2Num(c)
  " A bit of magic on empty strings
  if a:c == ""
    return 0
  endif
  exec 'let n = s:' . toupper(a:c)
  return n
endfun

function! Roman2Decimal(str)
  if a:str !~? '^[IVXLCDM]\+$'
    return a:str
  endif
  let sum = 0
  let i = 0
  let n0 = s:Char2Num(a:str[i])
  let len = strlen(a:str)
  while i < len
    let i = i + 1
    let n1 = s:Char2Num(a:str[i])
    " Magic: n1=0 when i exceeds len
    if n1 > n0
      let sum = sum - n0
    else
      let sum = sum + n0
    endif
    let n0 = n1
  endwhile
  return sum
endfun
" END: Roman2Decimal }}}


" BEGIN: Relative path {{{
function! CommonPath(path1, path2)
  let path1 = CleanupFileName(a:path1)
  let path2 = CleanupFileName(a:path2)
  return CommonString(path1, path2)
endfunction

function! CommonString(str1, str2)
  let str1 = a:str1
  let str2 = a:str2
  if str1 == str2
    return str1
  endif
  let n = 0
  while str1[n] == str2[n]
    let n = n+1
  endwhile
  return strpart(str1, 0, n)
endfunction

function! RelPathFromFile(srcFile, tgtFile)
  return RelPathFromDir(fnamemodify(a:srcFile, ':h'), a:tgtFile)
endfunction

function! RelPathFromDir(srcDir, tgtFile)
  let cleanDir = CleanupFileName(a:srcDir)
  let cleanFile = CleanupFileName(a:tgtFile)
  let cmnPath = CommonPath(cleanDir, cleanFile)
  let shortDir = strpart(cleanDir, strlen(cmnPath))
  let shortFile = strpart(cleanFile, strlen(cmnPath))
  let relPath = substitute(shortDir, '[^/]\+', '..', 'g')
  return relPath . '/' . shortFile
endfunction

" END: Relative path }}}


" BEGIN: Persistent settings {{{
if ! exists("g:genutilsNoPersist") || ! g:genutilsNoPersist
  " Make sure the '!' option to store global variables that are upper cased are
  "   stored in viminfo file.
  " Make sure it is the first option, so that it will not interfere with the
  "   'n' option ("Todd J. Cosgrove" (todd dot cosgrove at softechnics dot
  "   com)).
  " Also take care of empty value, when 'compatible' mode is on (Bram
  "   Moolenar)
  if &viminfo == ''
    set viminfo=!,'20,"50,h
  else
    set viminfo^=!
  endif
endif

function! PutPersistentVar(pluginName, persistentVar, value)
  if ! exists("g:genutilsNoPersist") || ! g:genutilsNoPersist
    let globalVarName = s:PersistentVarName(a:pluginName, a:persistentVar)
    exec 'let ' . globalVarName . " = '" . a:value . "'"
  endif
endfunction

function! GetPersistentVar(pluginName, persistentVar, default)
  if ! exists("g:genutilsNoPersist") || ! g:genutilsNoPersist
    let globalVarName = s:PersistentVarName(a:pluginName, a:persistentVar)
    if (exists(globalVarName))
      exec 'let value = ' . globalVarName
      exec 'unlet ' . globalVarName
    else
      let value = a:default
    endif
    return value
  else
    return default
  endif
endfunction

function! s:PersistentVarName(pluginName, persistentVar)
  return 'g:GU_' . toupper(a:pluginName) . '_' . toupper(a:persistentVar)
endfunction
" END: Persistent settings }}}


" FileChangedShell handling {{{
if !exists('s:fcShellPreFuncs')
  let s:fcShellPreFuncs = ''
  let s:fcShellFuncs = ''
endif

function! AddToFCShellPre(funcName)
  call RemoveFromFCShellPre(a:funcName)
  let s:fcShellPreFuncs = MvAddElement(s:fcShellPreFuncs, ',', a:funcName)
endfunction

function! RemoveFromFCShellPre(funcName)
  let s:fcShellPreFuncs = MvRemoveElement(s:fcShellPreFuncs, ',', a:funcName)
endfunction

function! AddToFCShell(funcName)
  call RemoveFromFCShell(a:funcName)
  let s:fcShellFuncs = MvAddElement(s:fcShellFuncs, ',', a:funcName)
endfunction

function! RemoveFromFCShell(funcName)
  let s:fcShellFuncs = MvRemoveElement(s:fcShellFuncs, ',', a:funcName)
endfunction

let s:defFCShellInstalled = 0
function! DefFCShellInstall()
  if ! s:defFCShellInstalled
    aug DefFCShell
    au!
    au FileChangedShell * nested call DefFileChangedShell()
    aug END
  endif
  let s:defFCShellInstalled = s:defFCShellInstalled + 1
endfunction

function! DefFCShellUninstall()
  if s:defFCShellInstalled <= 0
    return
  endif
  let s:defFCShellInstalled = s:defFCShellInstalled - 1
  if ! s:defFCShellInstalled
    aug DefFCShell
    au!
    aug END
  endif
endfunction

function! DefFileChangedShell()
  let autoread = s:InvokeFuncs(s:fcShellPreFuncs)
  let bufNo = expand('<abuf>') + 0
  let fName = expand('<afile>')
  let msg = ''
  if getbufvar(bufNo, '&modified')
    let msg = 'W12: Warning: File "' . fName .
          \ '" has changed and the buffer was changed in Vim as well'
  elseif ! autoread
    if ! filereadable(fName)
      let msg = 'E211: Warning: File "' . fName . '" no longer available'
    elseif filewritable(fName) == getbufvar(bufNo, '&readonly')
      let msg = 'W16: Warning: Mode of file "' . fName .
            \ '" has changed since editing started'
    else
      let msg = 'W11: Warning: File "' . fName .
            \ '" has changed since editing started'
    endif
  endif
  if msg != ''
    let option = confirm(msg, "&OK\n&Load File", 1, 'Warning')
  else " if autoread
    let option = 2
  endif
  if option == 2
    let orgWin = winnr()
    let orgBuf = bufnr('%')
    let _eventignore = &eventignore
    "set eventignore+=WinEnter,WinLeave,BufEnter,BufLeave
    call SaveWindowSettings2('DefFileChangedShell', 1)
    try
      if bufNo != winbufnr(0)
        exec 'split #' . bufNo
      endif
      call SaveSoftPosition('DefFileChangedShell')
      edit!
      call RestoreSoftPosition('DefFileChangedShell')
    finally
      if orgWin != winnr() || orgBuf != bufnr('%')
        quit
      endif
      call RestoreWindowSettings2('DefFileChangedShell')
      call ResetWindowSettings2('DefFileChangedShell')
    endtry
  endif
  call s:InvokeFuncs(s:fcShellFuncs)
  return (option == 2)
endfunction

function! s:InvokeFuncs(funcList)
  let autoread = &autoread
  if a:funcList != ''
    call MvIterCreate(a:funcList, ',', 'InvokeFuncs')
    while MvIterHasNext('InvokeFuncs') && ! autoread
      exec "let result = " . MvIterNext('InvokeFuncs') . '()'
      if result != -1
        let autoread = autoread || result
      endif
    endwhile
  endif
  return autoread
endfunction
" FileChangedShell handling }}}


" Sign related utilities {{{
function! CurLineHasSign()
  let signs = s:GetVimCmdOutput('sign place buffer=' . bufnr('%'), 1)
  return (match(signs,
        \ 'line=' . line('.') . '\s\+id=\d\+\s\+name=VimBreakPt') != -1)
endfunction

function! ClearAllSigns()
  let signs = GetVimCmdOutput('sign place buffer=' . bufnr('%'), 1)
  let curIdx = 0
  let pat = 'line=\d\+\s\+id=\zs\d\+\ze\s\+name=VimBreakPt'
  let id = 0
  while curIdx != -1 && curIdx < strlen(signs)
    let id = matchstr(signs, pat, curIdx)
    if id != ''
      exec 'sign unplace ' . id . ' buffer=' . bufnr('%')
    endif
    let curIdx = matchend(signs, pat, curIdx)
  endwhile
endfunction
" }}}
 
" Restore cpo.
let &cpo = s:save_cpo
unlet s:save_cpo

" vim6:fdm=marker et
