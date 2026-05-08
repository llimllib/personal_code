c = get_config()  # noqa

# Use emacs editing mode (enables Ctrl+A, Ctrl+E, etc.)
c.TerminalInteractiveShell.editing_mode = 'emacs'

# Enable C-x C-e to open `$EDITOR` with current cell
c.TerminalInteractiveShell.extra_open_editor_shortcuts = True
