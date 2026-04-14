import { CustomEditor, type ExtensionAPI } from "@mariozechner/pi-coding-agent";
import type { Keybindings, Theme, TUI } from "@mariozechner/pi-tui";

// Matches the reverse-video cursor block the editor emits:
//   \x1b[7m  +  one or more non-escape chars  +  \x1b[0m
// Using [^\x1b]* to avoid crossing into other escape sequences.
const CURSOR_RE = /\x1b\[7m([^\x1b]*)\x1b\[0m/;

class FocusCursorEditor extends CustomEditor {
  override render(width: number): string[] {
    // Strip the software cursor entirely so the hardware cursor (positioned by
    // the TUI at the CURSOR_MARKER the Editor emits) stands alone. Ghostty
    // renders it solid when focused, hollow when another window has OS focus.
    return super.render(width).map((line) => line.replace(CURSOR_RE, "$1"));
  }
}

export default function (pi: ExtensionAPI) {
  let savedTui: TUI | undefined;

  pi.on("session_start", (_event, ctx) => {
    ctx.ui.setEditorComponent(
      (tui: TUI, theme: Theme, keybindings: Keybindings) => {
        savedTui = tui;
        // Switch the TUI from software-only to hardware cursor mode.
        tui.setShowHardwareCursor(true);
        return new FocusCursorEditor(tui, theme, keybindings);
      },
    );
  });

  pi.on("session_end", () => {
    // Restore default cursor mode when the session exits.
    savedTui?.setShowHardwareCursor(false);
    savedTui = undefined;
  });
}
