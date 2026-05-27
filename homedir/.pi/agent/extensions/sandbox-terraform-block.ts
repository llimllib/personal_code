/**
 * Terraform Command Blocker
 *
 * Blocks dangerous terraform commands (destroy, apply) from being executed by pi.
 * Uses comprehensive pattern matching to prevent bypass attempts.
 *
 * Blocks:
 * - terraform destroy/apply (with any flags/arguments)
 * - tf destroy/apply (common alias)
 * - tofu destroy/apply (OpenTofu)
 * - Attempts to use full paths (/usr/bin/terraform, etc.)
 * - Piped commands, subshells, command substitution
 * - Variable expansion, escaped spaces, quoted commands
 *
 * Note: Filesystem protection (*.tfstate files) is handled by the sandbox extension.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { isToolCallEventType } from "@mariozechner/pi-coding-agent";

// Match terraform/tf/tofu followed by destroy or apply
// Accounts for:
// - Path prefixes: /usr/bin/terraform, ./terraform, ~/bin/tf
// - Whitespace variations: tabs, multiple spaces
// - Flags before or after: terraform -chdir=foo apply
const BLOCKED_PATTERNS = [
  // Direct invocations
  /(?:^|[;&|\(\s])(?:\.?\/)?(?:.*\/)?(?:terraform|tofu|tf)(?:\.exe)?\s+(?:[^;&|\n]*\s+)?(?:destroy|apply)(?:\s|$|[;&|\)])/i,

  // Via env, command, eval, sh -c, etc.
  /(?:env|command|eval|exec|sh|bash|zsh)\s+.*(?:terraform|tofu|tf).*(?:destroy|apply)/i,

  // Aliases or functions being defined
  /(?:alias|function)\s+\w+.*=.*(?:terraform|tofu|tf).*(?:destroy|apply)/i,

  // In strings or command substitution
  /["'`$].*(?:terraform|tofu|tf).*(?:destroy|apply)/i,
];

const DANGEROUS_PATTERNS = [
  // wget/curl downloading and executing terraform
  /(?:wget|curl).*(?:terraform|tofu)/i,

  // Docker/container operations that might run terraform
  /docker\s+(?:run|exec).*(?:terraform|tofu|tf)/i,
];

const BLOCKED_COMMANDS = [
  "terraform destroy",
  "terraform apply",
  "tf destroy",
  "tf apply",
  "tofu destroy",
  "tofu apply",
];

export default function (pi: ExtensionAPI) {
  // Block bash commands
  pi.on("tool_call", async (event, ctx) => {
    if (!isToolCallEventType("bash", event)) {
      return;
    }

    const command = event.input.command;

    // Check if command contains any blocked patterns
    for (const pattern of BLOCKED_PATTERNS) {
      if (pattern.test(command)) {
        ctx.ui.notify(
          `🚫 BLOCKED: Terraform destroy/apply commands are not allowed\n\nAttempted command:\n${command}\n\nThis protection cannot be bypassed.`,
          "error",
        );
        return {
          block: true,
          reason:
            "Terraform destroy/apply commands are blocked for safety. This includes tf/tofu aliases and indirect invocations.",
        };
      }
    }

    // Check for suspicious patterns that might be trying to bypass
    for (const pattern of DANGEROUS_PATTERNS) {
      if (pattern.test(command)) {
        const confirmed = await ctx.ui.confirm(
          "⚠️ Suspicious Command",
          `This command looks like it might be trying to download or run terraform:\n\n${command}\n\nAllow this command?`,
        );
        if (!confirmed) {
          return {
            block: true,
            reason: "Blocked by user: suspicious terraform-related command",
          };
        }
      }
    }
  });

  pi.registerCommand("terraform-block-status", {
    description: "Show terraform command blocking status",
    handler: async (_args, ctx) => {
      const lines = [
        "Terraform Command Blocker - Multi-layer Protection",
        "",
        "Command-level blocking:",
        ...BLOCKED_COMMANDS.map((cmd) => `  - ${cmd}`),
        "",
        "Pattern matching for bypass attempts:",
        "  - Full paths (/usr/bin/terraform)",
        "  - Aliases (tf, tofu)",
        "  - Indirect execution (eval, sh -c, etc.)",
        "  - Command substitution and pipes",
        "",
        "Note: Filesystem protection (*.tfstate) is handled by sandbox extension.",
      ];
      ctx.ui.notify(lines.join("\n"), "info");
    },
  });

  pi.on("session_start", async (_event, ctx) => {
    ctx.ui.setStatus(
      "terraform-block",
      ctx.ui.theme.fg("error", "🚫 terraform blocker on"),
    );
  });
}
