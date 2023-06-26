#!/usr/bin/env bash
#
# Generate a mermaid flow chart file and an SVG diagram with the dependencies
# of a monorepo
#
# remove all non-[a-zA-Z] characters because mermaid has strict limits on node
# names
function sanitize {
    echo "${1//[^a-zA-Z]/}"
}

# dependencies:
# brew install jq && npm install -g @mermaid-js/mermaid-cli
# https://mermaid.js.org/syntax/flowchart.html
printf "graph TD\n" > deps.mermaid
for package in packages/*; do
    # tell shellcheck that we're not actually trying to run a shell command
    # shellcheck disable=SC2089
    jqfilter='.dependencies | keys[] | split("/") | .[1]'
    for dep in $(jq -r "$jqfilter" "$package/package.json" 2>/dev/null); do
        # mermaid has very annoying rules about what characters are allowed in
        # node names. This isn't a complete solution, but should work most of
        # the time. It will fail if:
        # - a package is named "end", or any reserved mermaid word
        # - two packages have the same name modulo special characters
        pkgname=$(basename "$package")
        safepkg=$(sanitize "$pkgname")
        safedep=$(sanitize "$dep")
        printf '    %s["%s"] --> %s["%s"]\n' "$safepkg" "$pkgname" "$safedep" "$dep" >> deps.mermaid
    done
done

mmdc -i deps.mermaid -o deps.mermaid.svg

printf "wrote deps.mermaid.svg\n"
