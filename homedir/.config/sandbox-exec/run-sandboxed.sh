#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# run-sandboxed.sh
# Launch a command under sandbox-exec using ~/.config/sandbox-exec/agent.sb
# plus per-launch dynamic rules for the selected workdir and any sibling
# git worktrees. Modeled on Agent Safehouse's request → plan → render flow.
#
# Usage:
#   run-sandboxed.sh [--workdir=PATH] [--] CMD [ARGS...]
#
# Examples:
#   run-sandboxed.sh claude
#   run-sandboxed.sh --workdir=~/code/foo claude --resume
#   run-sandboxed.sh -- bash -lc 'cargo test'
# ---------------------------------------------------------------------------
set -euo pipefail

PROFILE="${HOME}/.config/sandbox-exec/agent.sb"
SANDBOX_EXEC="${SANDBOX_EXEC:-/usr/bin/sandbox-exec}"

if [[ ! -f "$PROFILE" ]]; then
  echo "run-sandboxed: missing profile: $PROFILE" >&2
  exit 2
fi
if [[ ! -x "$SANDBOX_EXEC" ]]; then
  echo "run-sandboxed: sandbox-exec not found at $SANDBOX_EXEC" >&2
  exit 2
fi

# --- Argument parsing ------------------------------------------------------
workdir=""
positional=()
while (($#)); do
  case "$1" in
    --workdir=*) workdir="${1#--workdir=}"; shift ;;
    --workdir)
      [[ $# -ge 2 ]] || { echo "run-sandboxed: --workdir needs a value" >&2; exit 2; }
      workdir="$2"; shift 2 ;;
    --help|-h)
      sed -n '2,15p' "$0"; exit 0 ;;
    --) shift; while (($#)); do positional+=("$1"); shift; done ;;
    *)  positional+=("$1"); shift ;;
  esac
done

if (( ${#positional[@]} == 0 )); then
  echo "run-sandboxed: no command given" >&2
  echo "usage: run-sandboxed.sh [--workdir=PATH] [--] CMD [ARGS...]" >&2
  exit 2
fi

# --- Resolve effective workdir --------------------------------------------
if [[ -z "$workdir" ]]; then
  workdir="$(pwd -P)"
fi
# Expand leading ~ if present.
case "$workdir" in
  "~"|"~/"*) workdir="${HOME}${workdir#~}" ;;
esac
if [[ ! -d "$workdir" ]]; then
  echo "run-sandboxed: workdir does not exist: $workdir" >&2
  exit 2
fi
# Normalize: absolute, symlink-resolved.
workdir="$(cd "$workdir" && pwd -P)"

# --- Resolve git context (worktree root, common dir, siblings) -------------
git_root=""
git_common=""
linked_worktrees=()
if git -C "$workdir" rev-parse --show-toplevel >/dev/null 2>&1; then
  git_root="$(git -C "$workdir" rev-parse --show-toplevel)"
  if [[ "$git_root" == "$workdir" ]]; then
    raw_common="$(git -C "$workdir" rev-parse --git-common-dir 2>/dev/null || true)"
    if [[ -n "$raw_common" ]]; then
      case "$raw_common" in
        /*) git_common="$raw_common" ;;
        *)  git_common="$(cd "$workdir/$raw_common" 2>/dev/null && pwd -P || true)" ;;
      esac
    fi
    while IFS= read -r line || [[ -n "$line" ]]; do
      [[ "$line" == worktree\ * ]] || continue
      p="${line#worktree }"
      [[ -n "$p" && -d "$p" ]] || continue
      [[ "$p" == "$workdir" ]] && continue
      linked_worktrees+=("$p")
    done < <(git -C "$workdir" worktree list --porcelain 2>/dev/null || true)
  fi
fi

# --- Build temp policy file ------------------------------------------------
tmp_policy="$(mktemp -t agent-sb.XXXXXXXX)"
cleanup() { rm -f "$tmp_policy"; }
trap cleanup EXIT INT TERM

# Mirrors Safehouse policy_render_build_path_ancestor_literals_block:
# emit (literal "/") plus a literal grant for every ancestor directory of
# the path. Using `literal` (not `subpath`) grants directory-listing on the
# ancestor itself but does NOT grant recursive read of its contents.
emit_ancestors() {
  local path="$1" label="$2" trimmed accum part
  printf ';; Generated ancestor directory literals for %s: %s\n' "$label" "$path"
  printf ';; (literal vs subpath: directory entry only, not recursive read.)\n'
  printf '(allow file-read*\n'
  printf '    (literal "/")\n'
  trimmed="${path#/}"
  accum=""
  local IFS='/'
  for part in $trimmed; do
    [[ -z "$part" ]] && continue
    accum="${accum}/${part}"
    printf '    (literal "%s")\n' "$accum"
  done
  printf ')\n\n'
}

{
  cat "$PROFILE"

  printf '\n;; ===========================================================================\n'
  printf ';; Wrapper-emitted dynamic rules (per-launch)\n'
  printf ';; workdir source: %s\n' "${workdir}"
  if [[ -n "$git_root" ]]; then
    printf ';; git worktree root: %s\n' "$git_root"
    [[ -n "$git_common" ]] && printf ';; git common dir:    %s\n' "$git_common"
    printf ';; linked worktrees: %d\n' "${#linked_worktrees[@]}"
  else
    printf ';; not a git repo (no worktree/common-dir grants)\n'
  fi
  printf ';; ===========================================================================\n\n'

  # 1. Selected workdir: ancestor reads + RW subpath.
  emit_ancestors "$workdir" "selected workdir"
  printf ';; #safehouse-test-id:workdir-grant# Selected workdir read+write.\n'
  printf '(allow file-read* file-write* (subpath "%s"))\n\n' "$workdir"

  # 2. Git common dir: RW only if it lives outside the workdir
  #    (i.e. the workdir is a linked worktree, not the main worktree).
  if [[ -n "$git_common" ]]; then
    case "$git_common" in
      "$workdir"|"$workdir"/*)
        printf ';; git common dir is inside the workdir; covered by the workdir grant.\n\n' ;;
      *)
        emit_ancestors "$git_common" "git worktree common dir"
        printf ';; #safehouse-test-id:worktree-common-dir-grant# Shared git metadata RW.\n'
        printf '(allow file-read* file-write* (subpath "%s"))\n\n' "$git_common"
        ;;
    esac
  fi

  # 3. Sibling worktrees: RO snapshot.
  if (( ${#linked_worktrees[@]} > 0 )); then
    printf ';; #safehouse-test-id:linked-worktrees-grant# Sibling worktrees (RO snapshot).\n'
    for p in "${linked_worktrees[@]}"; do
      emit_ancestors "$p" "sibling worktree"
      printf '(allow file-read* (subpath "%s"))\n\n' "$p"
    done
  fi
} > "$tmp_policy"

# --- Launch ---------------------------------------------------------------
exec "$SANDBOX_EXEC" -f "$tmp_policy" "${positional[@]}"
