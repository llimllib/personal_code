/**
 * fd tool - Overrides the built-in find tool
 *
 * This extension replaces the built-in find tool with fd for faster file searching.
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

const FindParams = Type.Object({
	pattern: Type.Optional(Type.String({ description: "File name pattern (regex)" })),
	path: Type.Optional(Type.String({ description: "Directory to search (default: current directory)" })),
	type: Type.Optional(Type.String({ description: "Type filter: 'f' (file), 'd' (directory), 'l' (symlink)" })),
	extension: Type.Optional(Type.String({ description: "File extension (e.g., 'ts', 'py')" })),
	hidden: Type.Optional(Type.Boolean({ description: "Include hidden files" })),
	glob: Type.Optional(Type.String({ description: "Treat `pattern` as a glob (e.g. '*.test.ts') instead of a regex" })),
	noIgnore: Type.Optional(
		Type.Boolean({ description: "Include files ignored by .gitignore (e.g. node_modules). Off by default." }),
	),
	maxDepth: Type.Optional(Type.Number({ description: "Maximum directory depth to descend" })),
	exclude: Type.Optional(Type.String({ description: "Glob to exclude. Repeat via comma: 'node_modules,dist'" })),
	fullPath: Type.Optional(Type.Boolean({ description: "Match the pattern against the whole path, not just the file name" })),
});

interface FindDetails {
	pattern?: string;
	path?: string;
	type?: string;
	extension?: string;
	hidden?: boolean;
	noIgnore?: boolean;
	fileCount: number;
	truncation?: TruncationResult;
	fullOutputPath?: string;
}

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "find",
		label: "fd",
		description: `Find files using fd. Much faster than find. Output is truncated to ${DEFAULT_MAX_LINES} lines or ${formatSize(DEFAULT_MAX_BYTES)} (whichever is hit first). If truncated, full output is saved to a temp file.`,
		parameters: FindParams,

		async execute(_toolCallId, params, signal, _onUpdate, ctx) {
			const { pattern, path, type, extension, hidden, glob, noIgnore, maxDepth, exclude, fullPath } = params;

			// Build fd command
			const args: string[] = [];

			if (type) {
				args.push("--type", type);
			}

			if (extension) {
				args.push("--extension", extension);
			}

			if (hidden) {
				args.push("--hidden");
			}

			// Without this, fd silently skips gitignored paths like node_modules,
			// which is the main reason a caller would fall back to bash find.
			if (noIgnore) {
				args.push("--no-ignore");
			}

			if (glob) {
				args.push("--glob");
			}

			if (maxDepth !== undefined) {
				args.push("--max-depth", String(maxDepth));
			}

			if (fullPath) {
				args.push("--full-path");
			}

			if (exclude) {
				for (const ex of exclude.split(",")) {
					const trimmed = ex.trim();
					if (trimmed) {
						args.push("--exclude", trimmed);
					}
				}
			}

			// Add pattern (or match-all if not provided). fd requires a pattern
			// before the path argument, otherwise it interprets the path as a pattern.
			args.push(glob || pattern || ".");

			// Add path
			if (path) {
				args.push(path);
			}

			let result;
			try {
				result = await pi.exec("fd", args, {
					cwd: ctx.cwd,
					signal,
				});
			} catch (err: any) {
				throw new Error(`fd execution failed: ${err.message}`);
			}

			if (result.code !== 0) {
				throw new Error(`fd failed with exit code ${result.code}: ${result.stderr}`);
			}

			if (!result.stdout.trim()) {
				return {
					content: [{ type: "text", text: "No files found" }],
					details: {
						pattern,
						path,
						type,
						extension,
						hidden,
						noIgnore,
						fileCount: 0,
					} as FindDetails,
				};
			}

			// Apply truncation
			const truncation = truncateHead(result.stdout, {
				maxLines: DEFAULT_MAX_LINES,
				maxBytes: DEFAULT_MAX_BYTES,
			});

			// Count files (each non-empty line)
			const fileCount = result.stdout.split("\n").filter((line) => line.trim()).length;

			const details: FindDetails = {
				pattern,
				path,
				type,
				extension,
				hidden,
				noIgnore,
				fileCount,
			};

			let resultText = truncation.content;

			if (truncation.truncated) {
				// Save full output to temp file
				const tempDir = await mkdtemp(join(tmpdir(), "pi-fd-"));
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
			let text = theme.fg("toolTitle", theme.bold("fd "));
			
			if (args.pattern) {
				text += theme.fg("accent", args.pattern);
			}

			if (args.path && args.path !== ".") {
				text += theme.fg("muted", ` in ${args.path}`);
			}

			const filters: string[] = [];
			if (args.type) filters.push(`--type ${args.type}`);
			if (args.extension) filters.push(`--extension ${args.extension}`);
			if (args.hidden) filters.push("--hidden");

			if (filters.length > 0) {
				text += theme.fg("dim", ` ${filters.join(" ")}`);
			}

			return new Text(text, 0, 0);
		},

		renderResult(result, { expanded, isPartial }, theme, _context) {
			const details = result.details as FindDetails | undefined;

			if (isPartial) {
				return new Text(theme.fg("warning", "Searching..."), 0, 0);
			}

			if (!details || details.fileCount === 0) {
				return new Text(theme.fg("dim", "No files found"), 0, 0);
			}

			let text = theme.fg("success", `${details.fileCount} files`);

			if (details.truncation?.truncated) {
				text += theme.fg("warning", " (truncated)");
			}

			if (expanded) {
				const content = result.content[0];
				if (content?.type === "text") {
					// Show first 30 lines in expanded view
					const lines = content.text.split("\n").slice(0, 30);
					for (const line of lines) {
						text += `\n${theme.fg("dim", line)}`;
					}
					if (content.text.split("\n").length > 30) {
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
