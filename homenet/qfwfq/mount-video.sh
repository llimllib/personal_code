#!/usr/bin/env bash
# Keep shifgrethor's video share mounted at /Volumes/video for Jellyfin.
# launchd (org.billmill.mount-video) runs this every minute and on network
# changes. When the share is mounted it does nothing.
#
# The password is in the login keychain; add it once with:
#   security add-generic-password -s org.billmill.mount-video -a llimllib -T /usr/bin/security -w
set -uo pipefail

server=shifgrethor.local
share=video
user=llimllib
service=org.billmill.mount-video
mountpoint=/Volumes/$share
state=$HOME/.local/state/mount-video
max_backoff=3600

log() { printf '%s %s\n' "$(date '+%Y-%m-%dT%H:%M:%S')" "$*"; }

mounted() { mount | grep -qF " on $mountpoint (smbfs"; }

if mounted; then
    rm -f "$state"
    exit 0
fi

# After a failure, back off exponentially (1m, 2m, 4m... up to an hour).
# Repeated bad logins can make the Synology's Auto Block ban qfwfq.
failures=0 next=0
if [ -f "$state" ]; then
    read -r failures next <"$state" || true
fi
now=$(date +%s)
if [ "$now" -lt "${next:-0}" ]; then
    exit 0
fi

fail() {
    failures=$((failures + 1))
    delay=$((60 * 2 ** (failures - 1)))
    [ "$delay" -gt "$max_backoff" ] && delay=$max_backoff
    mkdir -p "$(dirname "$state")"
    echo "$failures $((now + delay))" >"$state"
    log "$* (failure $failures, next try in ${delay}s)"
    exit 1
}

# Fail fast if the server is down, rather than letting mount hang
if ! nc -z -G 5 "$server" 445 >/dev/null 2>&1; then
    fail "$server:445 unreachable"
fi

if ! pw=$(security find-generic-password -s "$service" -a "$user" -w 2>/dev/null); then
    fail "no keychain item for service $service account $user"
fi

# osascript's mount volume creates the mount point under /Volumes without root,
# as Finder does. The password goes in through the environment, not argv
log "mounting smb://$server/$share"
if ! out=$(PW=$pw osascript - "smb://$server/$share" "$user" 2>&1 <<'EOF'
on run argv
	mount volume (item 1 of argv) as user name (item 2 of argv) with password (system attribute "PW")
end run
EOF
); then
    fail "mount failed: $out"
fi

if ! mounted; then
    # e.g. a leftover /Volumes/video made macOS mount it at /Volumes/video-1
    fail "mounted, but not at $mountpoint: $(mount | grep -F "$server/$share")"
fi
rm -f "$state"
log "mounted at $mountpoint"
