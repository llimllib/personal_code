/**
 * Tool preferences - instructs the LLM to prefer find and grep tools over bash
 *
 * This extension adds instructions to the system prompt encouraging the LLM
 * to use the find and grep tools directly instead of bash one-liners.
 * Only injects the message once at session start.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.on("session_start", async (_event, ctx) => {
    // Send a message that will be included in context for all future turns
    pi.sendMessage(
      {
        customType: "tool-preferences",
        content: `## Tool Preferences

- Prefer the \`find\` and \`grep\` tools over bash commands with find/grep
- Use \`find\` for discovering files by name or type
- Use \`grep\` for searching file contents`,
        display: false, // Don't show in UI
      },
      { deliverAs: "nextTurn" },
    );
  });
}
