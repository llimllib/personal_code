import os
import random
import shlex
import subprocess

outputs = ["/Volumes/KIDPOD 1/Podcasts/", "/Volumes/KIDPOD 2/Podcasts/"]

for pod in os.listdir("podcasts"):
    if pod.startswith("."):
        continue
    podcasts = os.listdir(f"podcasts/{pod}")
    # print(podcasts[:5])
    somepods = random.sample(podcasts, 20)
    for device in outputs:
        for f in somepods:
            cmd = f"rsync './podcasts/{pod}/{f}' '{device}/{pod}/'"
            # print(cmd)
            subprocess.run(shlex.split(cmd), check=True)
