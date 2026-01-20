# Docker Deployment Guide for Labyrinth Server

## Prerequisites

- Docker installed on your server
- Docker Compose installed (optional, but recommended)
- Port 8082 open in your firewall
- Domain `game.clxmi.com` pointing to your server's public IP

## Quick Start

### Using Docker Compose (Recommended)

1. Navigate to the Labyrinth.Server directory:
   ```bash
   cd Labyrinth.Server
   ```

2. Build and start the server:
   ```bash
   docker-compose up -d
   ```

3. Check logs:
   ```bash
   docker-compose logs -f
   ```

4. Stop the server:
   ```bash
   docker-compose down
   ```

### Using Docker directly

1. Build the image (from the repository root):
   ```bash
   docker build -f Labyrinth.Server/Dockerfile -t labyrinth-server .
   ```

2. Run the container:
   ```bash
   docker run -d \
     --name labyrinth-server \
     -p 8082:8082 \
     -e SERVER_ADDRESS=0.0.0.0 \
     -e SERVER_NAME=Group1 \
     -e SERVER_PUBLICHOST=game.clxmi.com \
     -e MANAGEMENT_API_URL=https://game.clxmi.com \
     labyrinth-server
   ```

3. Check logs:
   ```bash
   docker logs -f labyrinth-server
   ```

4. Stop the container:
   ```bash
   docker stop labyrinth-server
   docker rm labyrinth-server
   ```

## Configuration

You can customize the server by setting environment variables in `docker-compose.yml` or passing them with `-e` flags:

- `SERVER_NAME`: Name displayed in the server browser (default: "Group1")
- `SERVER_PORT`: Internal port (default: 8082)
- `SERVER_PUBLICHOST`: Public hostname/IP for clients to connect (default: "game.clxmi.com")
- `SERVER_WSENDPOINT`: WebSocket endpoint path (default: "/game")
- `MANAGEMENT_API_URL`: Management server URL (default: "https://game.clxmi.com")

## Networking Requirements

### Firewall Configuration

Allow incoming TCP connections on port 8082:

**Ubuntu/Debian (ufw):**
```bash
sudo ufw allow 8082/tcp
```

**CentOS/RHEL (firewalld):**
```bash
sudo firewall-cmd --permanent --add-port=8082/tcp
sudo firewall-cmd --reload
```

**iptables:**
```bash
sudo iptables -A INPUT -p tcp --dport 8082 -j ACCEPT
sudo iptables-save
```

### Router Configuration

If your server is behind a router, forward port 8082:
- External port: 8082
- Internal port: 8082
- Protocol: TCP
- Internal IP: Your server's local IP

### DNS Configuration

Ensure `game.clxmi.com` resolves to your server's public IP:
```bash
dig game.clxmi.com
# Should return your server's public IP
```

## Verification

1. Check if the server is running:
   ```bash
   docker ps | grep labyrinth-server
   ```

2. Check if the port is listening:
   ```bash
   netstat -tlnp | grep 8082
   # or
   ss -tlnp | grep 8082
   ```

3. Test WebSocket connection locally:
   ```bash
   curl -i -N -H "Connection: Upgrade" -H "Upgrade: websocket" \
     -H "Sec-WebSocket-Version: 13" -H "Sec-WebSocket-Key: test" \
     http://localhost:8082/game
   ```

4. Check server logs for registration:
   ```bash
   docker-compose logs | grep "Registering server"
   ```

   You should see:
   ```
   === Registering server with Management Server ===
   Server name: Group1
   Public URI: ws://game.clxmi.com:8082/game
   ```

## Troubleshooting

### Container won't start
```bash
# Check logs for errors
docker-compose logs

# Rebuild the image
docker-compose build --no-cache
docker-compose up -d
```

### Connection refused from clients
1. Verify port 8082 is exposed: `docker ps` should show `0.0.0.0:8082->8082/tcp`
2. Check firewall rules (see Firewall Configuration above)
3. Verify router port forwarding if behind NAT
4. Check DNS: `dig game.clxmi.com` should resolve to your public IP

### Server not appearing in browser
1. Check management server connection in logs: `docker-compose logs | grep "Management"`
2. Verify `MANAGEMENT_API_URL` is correct
3. Ensure server status is LOBBY (not IN_GAME or FINISHED)

## Updating

1. Stop the current container:
   ```bash
   docker-compose down
   ```

2. Pull latest code and rebuild:
   ```bash
   git pull
   docker-compose build --no-cache
   docker-compose up -d
   ```

## Production Recommendations

1. **Use a reverse proxy** (nginx/Traefik) for SSL/TLS termination
2. **Set resource limits** in docker-compose.yml:
   ```yaml
   deploy:
     resources:
       limits:
         cpus: '2'
         memory: 2G
   ```
3. **Enable logging drivers** for centralized log management
4. **Use Docker secrets** for sensitive configuration
5. **Set up monitoring** (Prometheus, Grafana)
6. **Configure automatic restarts** (already set: `restart: unless-stopped`)
