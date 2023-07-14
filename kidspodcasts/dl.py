#!/usr/bin/env python
import os
import re
import shlex
import subprocess
import unicodedata

import feedparser

pods = [
    ("circle round", "https://rss.wbur.org/CircleRound/podcast"),
    ("stories podcast", "https://rss.art19.com/stories-podcast"),
    (
        "super great kids stories",
        "https://rss.acast.com/super-great-kids-stories",
    ),
    ("wow in the world", "https://rss.art19.com/wow-in-the-world"),
    ("flip and mozi", "https://rss.art19.com/guide-to-how-to-be-an-earthling-"),
    ("who, when, wow", "https://rss.art19.com/who-when-wow"),
]


def sanitize(value, allow_unicode=False):
    """
    Taken from https://github.com/django/django/blob/master/django/utils/text.py
    Convert to ASCII if 'allow_unicode' is False. Convert spaces or repeated
    dashes to single dashes. Remove characters that aren't alphanumerics,
    underscores, or hyphens. Convert to lowercase. Also strip leading and
    trailing whitespace, dashes, and underscores.
    """
    value = str(value)
    if allow_unicode:
        value = unicodedata.normalize("NFKC", value)
    else:
        value = (
            unicodedata.normalize("NFKD", value)
            .encode("ascii", "ignore")
            .decode("ascii")
        )
    value = re.sub(r"[^\w\s-]", "", value.lower())
    return re.sub(r"[-\s]+", "-", value).strip("-_")


def getext(s):
    """
    to get the extension, minus any http garbage:
    - split the string by dots
    - in the last group, get the leading printable characters
    - return them prefixed by a dot

    this is terrible but it works well enough in our cases so far

    getext("test.case.mp3?http_param=here") -> ".mp3"
    """
    return "." + re.search(r"(^\w+)", s.split(".")[-1]).groups(0)[0]


def main():
    for title, pod_url in pods:
        outdir = f"podcasts/{sanitize(title)}"
        os.makedirs(outdir, exist_ok=True)
        f = feedparser.parse(pod_url)
        for entry in f.entries:
            for link in entry["links"]:
                if link["type"] == "audio/mpeg" or link["href"].endswith(".mp3"):
                    extension = getext(link["href"])
                    outfile = f'{outdir}/{sanitize(entry["title"])}{extension}'
                    if not os.path.isfile(outfile):
                        cmd = f"curl --location -o {outfile} {link['href']}"
                        print(cmd)
                        try:
                            subprocess.run(shlex.split(cmd), check=True)
                        except subprocess.CalledProcessError:
                            # retry once if the process fails
                            subprocess.run(shlex.split(cmd), check=True)
                    break


if __name__ == "__main__":
    main()
