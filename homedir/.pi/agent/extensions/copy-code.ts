/**
 * Copy Code Extension
 *
 * Provides a /cp command that copies the most recent code block from the
 * assistant's last message to the system clipboard.
 *
 * Usage:
 * 1. After receiving a message with code blocks, run /cp
 * 2. The last code block will be copied to your clipboard
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { execSync } from "child_process";

export default function copyCodeExtension(pi: ExtensionAPI) {
  pi.registerCommand("cp", {
    description: "Copy the most recent code block to clipboard",
    handler: async (args, ctx) => {
      // Get the current branch (session history)
      const branch = ctx.sessionManager.getBranch();

      // Find the last assistant message
      let lastAssistantMessage = null;
      for (let i = branch.length - 1; i >= 0; i--) {
        const entry = branch[i];
        if (entry.type === "message" && entry.message.role === "assistant") {
          lastAssistantMessage = entry.message;
          break;
        }
      }

      if (!lastAssistantMessage) {
        ctx.ui.notify("No assistant message found", "error");
        return;
      }

      // Extract text content from the message
      let fullText = "";
      for (const block of lastAssistantMessage.content) {
        if (block.type === "text") {
          fullText += block.text;
        }
      }

      // Find all code blocks (```...```)
      const codeBlockRegex = /```[\w]*\n(.*?)```/gs;
      const matches = [...fullText.matchAll(codeBlockRegex)];

      if (matches.length === 0) {
        ctx.ui.notify("No code blocks found in last message", "error");
        return;
      }

      // Get the last code block
      const lastCodeBlock = matches[matches.length - 1][1].trim();

      try {
        // Copy to clipboard with platform detection
        let copyCommand: string;
        const platform = process.platform;

        if (platform === "darwin") {
          // macOS
          copyCommand = "pbcopy";
        } else if (platform === "win32") {
          // Windows
          copyCommand = "clip";
        } else {
          // Linux - try xclip first, fall back to xsel
          try {
            execSync("which xclip", { stdio: "ignore" });
            copyCommand = "xclip -selection clipboard";
          } catch {
            try {
              execSync("which xsel", { stdio: "ignore" });
              copyCommand = "xsel --clipboard --input";
            } catch {
              ctx.ui.notify(
                "No clipboard tool found (install xclip or xsel)",
                "error",
              );
              return;
            }
          }
        }

        execSync(copyCommand, { input: lastCodeBlock });

        const lines = lastCodeBlock.split("\n").length;
        ctx.ui.notify(
          `Copied ${lines} line${lines !== 1 ? "s" : ""} to clipboard`,
          "info",
        );
      } catch (error) {
        ctx.ui.notify(`Failed to copy: ${error}`, "error");
      }
    },
  });
}
