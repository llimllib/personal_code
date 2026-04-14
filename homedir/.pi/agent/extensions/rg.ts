/**
 * ripgrep (rg) tool - Overrides the built-in grep tool
 *
 * This extension replaces the built-in grep tool with ripgrep for faster searching.
 * Follows proper truncation guidelines to avoid overwhelming the LLM context.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import {
	DEFAULT_MAX_BYTES,
	DEFAULT_MAX_LINES,
	formatSize,
	truncateHead,
	type TruncationResult,
} from "@mariozechner/pi-coding-agent";
import { Type } from "@sinclair/typebox";
import { Text } from "@mariozechner/pi-tui";
import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

const GrepParams = Type.Object({
	pattern: Type.String({ description: "Search pattern (regex)" }),
	path: Type.Optional(Type.String({ description: "Directory or file to search (default: current directory)" })),
	filePattern: Type.Optional(Type.String({ description: "File glob pattern, e.g. '*.ts'" })),
	ignoreCase: Type.Optional(Type.Boolean({ description: "Case-insensitive search" })),
});

interface GrepDetails {
	pattern: string;
	path?: string;
	filePattern?: string;
	ignoreCase?: boolean;
	matchCount: number;
	truncation?: TruncationResult;
	fullOutputPath?: string;
}

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "grep",
		label: "ripgrep",
		description: `Search file contents using ripgrep. Output is truncated to ${DEFAULT_MAX_LINES} lines or ${formatSize(DEFAULT_MAX_BYTES)} (whichever is hit first). If truncated, full output is saved to a temp file.`,
		parameters: GrepParams,

		async execute(_toolCallId, params, signal, _onUpdate, ctx) {
			const { pattern, path, filePattern, ignoreCase } = params;

			// Build ripgrep command
			const args = [
				"--line-number",
				"--color=never",
				"--no-heading",
			];

			if (ignoreCase) {
				args.push("--ignore-case");
			}

			if (filePattern) {
				args.push("--glob", filePattern);
			}

			args.push(pattern);
			args.push(path || ".");

			let result;
			try {
				result = await pi.exec("rg", args, {
					cwd: ctx.cwd,
					signal,
				});
			} catch (err: any) {
				throw new Error(`ripgrep execution failed: ${err.message}`);
			}

			// ripgrep exits with 1 when no matches found
			if (result.code === 1 || !result.stdout.trim()) {
				return {
					content: [{ type: "text", text: "No matches found" }],
					details: {
						pattern,
						path,
						filePattern,
						ignoreCase,
						matchCount: 0,
					} as GrepDetails,
				};
			}

			if (result.code !== 0) {
				throw new Error(`ripgrep failed with exit code ${result.code}: ${result.stderr}`);
			}

			// Apply truncation
			const truncation = truncateHead(result.stdout, {
				maxLines: DEFAULT_MAX_LINES,
				maxBytes: DEFAULT_MAX_BYTES,
			});

			// Count matches (each non-empty line)
			const matchCount = result.stdout.split("\n").filter((line) => line.trim()).length;

			const details: GrepDetails = {
				pattern,
				path,
				filePattern,
				ignoreCase,
				matchCount,
			};

			let resultText = truncation.content;

			if (truncation.truncated) {
				// Save full output to temp file
				const tempDir = await mkdtemp(join(tmpdir(), "pi-rg-"));
				const tempFile = join(tempDir, "output.txt");
				await writeFile(tempFile, result.stdout, "utf8");

				details.truncation = truncation;
				details.fullOutputPath = tempFile;

				// Add truncation notice
				const truncatedLines = truncation.totalLines - truncation.outputLines;
				const truncatedBytes = truncation.totalBytes - truncation.outputBytes;

				resultText += `\n\n[Output truncated: showing ${truncation.outputLines} of ${truncation.totalLines} lines`;
				resultText += ` (${formatSize(truncation.outputBytes)} of ${formatSize(truncation.totalBytes)}).`;
				resultText += ` ${truncatedLines} lines (${formatSize(truncatedBytes)}) omitted.`;
				resultText += ` Full output saved to: ${tempFile}]`;
			}

			return {
				content: [{ type: "text", text: resultText }],
				details,
			};
		},

		renderCall(args, theme, _context) {
			let text = theme.fg("toolTitle", theme.bold("rg "));
			text += theme.fg("accent", `"${args.pattern}"`);
			if (args.path) {
				text += theme.fg("muted", ` in ${args.path}`);
			}
			if (args.filePattern) {
				text += theme.fg("dim", ` --glob ${args.filePattern}`);
			}
			if (args.ignoreCase) {
				text += theme.fg("dim", " -i");
			}
			return new Text(text, 0, 0);
		},

		renderResult(result, { expanded, isPartial }, theme, _context) {
			const details = result.details as GrepDetails | undefined;

			if (isPartial) {
				return new Text(theme.fg("warning", "Searching..."), 0, 0);
			}

			if (!details || details.matchCount === 0) {
				return new Text(theme.fg("dim", "No matches found"), 0, 0);
			}

			let text = theme.fg("success", `${details.matchCount} matches`);

			if (details.truncation?.truncated) {
				text += theme.fg("warning", " (truncated)");
			}

			if (expanded) {
				const content = result.content[0];
				if (content?.type === "text") {
					// Show first 20 lines in expanded view
					const lines = content.text.split("\n").slice(0, 20);
					for (const line of lines) {
						text += `\n${theme.fg("dim", line)}`;
					}
					if (content.text.split("\n").length > 20) {
						text += `\n${theme.fg("muted", "... (use read tool to see full output)")}`;
					}
				}

				if (details.fullOutputPath) {
					text += `\n${theme.fg("dim", `Full output: ${details.fullOutputPath}`)}`;
				}
			}

			return new Text(text, 0, 0);
		},
	});
}
