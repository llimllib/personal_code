#!/usr/bin/env bash
#
# Generate a graphviz dot file and an SVG diagram with the dependencies of a monorepo

#prereqs: brew install jq graphviz 
# docs for these options: https://graphviz.org/pdf/dot.1.pdf
cat <<EOF > deps.dot
digraph G {
node[fontsize=9]
layout=neato
overlap=scalexy; //false, compress, ...
sep="+1"; // 0.1, +1
EOF
for package in packages/*; do
    # tell shellcheck that we're not actually trying to run a shell command
    # shellcheck disable=SC2089
    jqfilter='.dependencies | keys[] | split("/") | .[1]'
    for dep in $(jq -r "$jqfilter" "$package/package.json" 2>/dev/null); do
        printf '    "%s" -> "%s"\n' "$(basename "$package")" "$dep" >> deps.dot
    done
done

printf "}\n" >> deps.dot

# render the dot file to an svg
dot -Tsvg -o deps.graphviz.svg deps.dot

printf "wrote deps.graphviz.svg\n"
