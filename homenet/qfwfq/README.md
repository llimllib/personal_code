# qfwfq

Caddy on qfwfq (a Mac mini) serves internal services at
`https://<name>.home.billmill.org` with a real Let's Encrypt certificate. They
work on the home LAN without Tailscale, and from anywhere on the tailnet.

| service  | url                                  | upstream         |
|----------|--------------------------------------|------------------|
| jellyfin | https://jellyfin.home.billmill.org   | `localhost:8096` |
| grafana  | https://grafana.home.billmill.org    | `localhost:3000` |

## How it fits together

```mermaid
flowchart LR
    subgraph internet[Internet]
        dns["DigitalOcean DNS<br/>*.home.billmill.org A 192.168.68.56"]
        le[Let's Encrypt]
    end

    away["Tailnet device<br/>away from home"]

    subgraph lan["Home LAN (Deco)"]
        client[LAN device]
        subgraph qfwfq["qfwfq 192.168.68.56"]
            ts["Tailscale<br/>subnet route 192.168.68.56/32"]
            caddy["Caddy :443<br/>wildcard cert"]
            jellyfin["jellyfin<br/>localhost:8096"]
            grafana["grafana<br/>localhost:3000"]
        end
    end

    client -. "lookup name" .-> dns
    away -. "lookup name" .-> dns
    client -- HTTPS --> caddy
    away -- "HTTPS over tailnet" --> ts --> caddy
    caddy -- jellyfin.home --> jellyfin
    caddy -- grafana.home --> grafana
    caddy -. "DNS-01 TXT record via API" .-> dns
    le -. "checks TXT record" .-> dns
    caddy -. "ACME" .-> le
```

- **DNS**: a public wildcard A record `*.home.billmill.org -> 192.168.68.56`
  (qfwfq's LAN IP) in DigitalOcean. The Deco has a DHCP reservation that keeps
  qfwfq at that address.
- **TLS**: one wildcard certificate, obtained with a DNS-01 challenge through
  DigitalOcean's API. Let's Encrypt can't reach a LAN IP, so the HTTP
  challenge won't work. This needs a caddy build with the
  [caddy-dns/digitalocean](https://github.com/caddy-dns/digitalocean) plugin.
- **Away from home**: qfwfq advertises `192.168.68.56/32` as a Tailscale
  subnet route, so tailnet devices reach the LAN IP from anywhere. It's a /32,
  not the whole /22, because other Deco networks use the same 192.168.68.x
  range by default.

## Files

- `Caddyfile` -> `~/.config/caddy/Caddyfile`
- `org.billmill.caddy.plist` -> `~/Library/LaunchAgents/`, a launchd agent
  that starts caddy at login and restarts it if it dies. It logs to
  `~/Library/Logs/caddy.log`
- `env.example` -> `~/.config/caddy/env`, mode 600, with a DigitalOcean token
  that has the domain create, read, and delete scopes. Not committed
- `install.sh` copies the config over ssh, validates it, and restarts caddy.
  `--upgrade` also downloads a new caddy build to `~/bin/caddy`

## Adding a service

Add a matcher and handler to the Caddyfile, then run `./install.sh`. DNS and
the certificate already cover it:

```
	@foo host foo.home.billmill.org
	handle @foo {
		reverse_proxy localhost:1234
	}
```

## One-time setup (already done)

```sh
# DNS
doctl compute domain records create billmill.org \
  --record-type A --record-name '*.home' --record-data 192.168.68.56 --record-ttl 300

# Tailscale subnet route, then approve it in the admin console. List the exit
# node explicitly, because it lives in the same route list
/Applications/Tailscale.app/Contents/MacOS/Tailscale set \
  --advertise-routes=192.168.68.56/32 --advertise-exit-node
```

## Rotating the token

Put the new token in `~/.config/caddy/env` on qfwfq, then run `./install.sh`.
Caddy renews the certificate about 30 days before it expires, so a token that
expires sooner than that breaks renewal.
