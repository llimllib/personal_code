#!/usr/bin/env bash
# Install or update Caddy on qfwfq, from any machine that can ssh to it.
#
#   ./install.sh            push the Caddyfile and plist, restart caddy
#   ./install.sh --upgrade  also download the latest caddy build
#
# The token file (~/.config/caddy/env on qfwfq) is never touched; create it
# from env.example before the first run.
set -euo pipefail

HOST=${HOST:-qfwfq}
DIR=$(cd "$(dirname "$0")" && pwd)
LABEL=org.billmill.caddy

upgrade=false
case "${1:-}" in
"") ;;
--upgrade) upgrade=true ;;
*)
    echo "usage: $0 [--upgrade]" >&2
    exit 1
    ;;
esac

ssh "$HOST" 'mkdir -p ~/bin ~/.config/caddy ~/Library/LaunchAgents ~/Library/Logs'
scp -q "$DIR/Caddyfile" "$HOST:.config/caddy/Caddyfile"
scp -q "$DIR/$LABEL.plist" "$HOST:Library/LaunchAgents/$LABEL.plist"

# ssh joins its arguments into one string for the remote shell, so only pass
# shell-safe words; everything else lives in the heredoc
ssh "$HOST" bash -s -- "$upgrade" "$LABEL" <<'EOF'
set -euo pipefail
upgrade=$1 label=$2
domain=gui/$(id -u)
# Homebrew's caddy has no DNS plugins; caddyserver.com builds one with the
# DigitalOcean provider, which the wildcard certificate needs
url="https://caddyserver.com/api/download?os=darwin&arch=arm64&p=github.com/caddy-dns/digitalocean"

if [ "$upgrade" = true ] || [ ! -x ~/bin/caddy ]; then
    echo "downloading caddy"
    curl -fsSL -o ~/bin/caddy.new "$url"
    chmod +x ~/bin/caddy.new
    if ! ~/bin/caddy.new list-modules | grep -q '^dns.providers.digitalocean$'; then
        echo "downloaded caddy is missing the digitalocean module" >&2
        exit 1
    fi
    mv ~/bin/caddy.new ~/bin/caddy
fi
~/bin/caddy version

if [ ! -f ~/.config/caddy/env ]; then
    echo "missing ~/.config/caddy/env on $(hostname); create it from env.example" >&2
    exit 1
fi
chmod 600 ~/.config/caddy/env

if ! out=$(~/bin/caddy validate --config ~/.config/caddy/Caddyfile --envfile ~/.config/caddy/env 2>&1); then
    echo "$out" >&2
    exit 1
fi

# bootout + bootstrap rather than kickstart, so plist changes take effect
launchctl bootout "$domain/$label" 2>/dev/null || true
for _ in 1 2 3 4 5 6 7 8 9 10; do
    launchctl print "$domain/$label" >/dev/null 2>&1 || break
    sleep 1
done
launchctl bootstrap "$domain" ~/Library/LaunchAgents/"$label".plist
sleep 2
launchctl print "$domain/$label" | grep -E '^\s+state ='
EOF
