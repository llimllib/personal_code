#!/usr/bin/env bash
set -euo pipefail

i=0
j=0
while read -r -a line; do
    ltr=${line[1]//:/}

    # sed outputs a newline and I couldn't figure out how to suppress it, so subract one
    ltrcount=$(($(echo "${line[2]}" | sed "s/[^${ltr}]//g" | wc -c)-1))

    min=${line[0]//-*/}
    max=${line[0]//*-/}
    if [[ $min -le $ltrcount ]] && [[ $max -ge $ltrcount ]]; then
        i=$((i+1))
    fi

    a=$([[ ${ltr} == "${line[2]:$((min-1)):1}" ]] && echo "true" || echo "false")
    b=$([[ ${ltr} == "${line[2]:$((max-1)):1}" ]] && echo "true" || echo "false")
    if [[ "$a" != "$b" ]]; then
        j=$((j+1))
    fi
done < input.txt
echo $i
echo $j
