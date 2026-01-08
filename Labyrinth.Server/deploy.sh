#!/bin/bash

# Labyrinth Server Docker Deployment Script

set -e

cd "$(dirname "$0")"

echo "=== Labyrinth Server Docker Deployment ==="
echo ""

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo "Error: Docker is not installed. Please install Docker first."
    exit 1
fi

# Check if Docker Compose is installed
if ! command -v docker-compose &> /dev/null; then
    echo "Error: Docker Compose is not installed. Please install Docker Compose first."
    exit 1
fi

# Parse arguments
ACTION="${1:-up}"

case "$ACTION" in
    up|start)
        echo "Building and starting Labyrinth Server..."
        docker-compose up -d --build
        echo ""
        echo "Server started successfully!"
        echo "Checking logs in 3 seconds..."
        sleep 3
        docker-compose logs --tail=50
        echo ""
        echo "To view live logs: docker-compose logs -f"
        ;;

    down|stop)
        echo "Stopping Labyrinth Server..."
        docker-compose down
        echo "Server stopped."
        ;;

    restart)
        echo "Restarting Labyrinth Server..."
        docker-compose down
        docker-compose up -d --build
        echo "Server restarted. Checking logs..."
        sleep 3
        docker-compose logs --tail=50
        ;;

    logs)
        echo "Showing Labyrinth Server logs (Ctrl+C to exit)..."
        docker-compose logs -f
        ;;

    status)
        echo "Labyrinth Server status:"
        docker-compose ps
        echo ""
        echo "Port status:"
        if command -v ss &> /dev/null; then
            ss -tlnp | grep :8082 || echo "Port 8082 not listening"
        else
            netstat -tlnp | grep :8082 || echo "Port 8082 not listening"
        fi
        ;;

    rebuild)
        echo "Rebuilding Labyrinth Server (no cache)..."
        docker-compose down
        docker-compose build --no-cache
        docker-compose up -d
        echo "Server rebuilt and started."
        ;;

    clean)
        echo "Cleaning up Docker resources..."
        docker-compose down -v
        docker system prune -f
        echo "Cleanup complete."
        ;;

    *)
        echo "Usage: $0 {up|down|restart|logs|status|rebuild|clean}"
        echo ""
        echo "Commands:"
        echo "  up/start   - Build and start the server"
        echo "  down/stop  - Stop the server"
        echo "  restart    - Restart the server"
        echo "  logs       - View live logs"
        echo "  status     - Show server status"
        echo "  rebuild    - Rebuild without cache and restart"
        echo "  clean      - Stop and remove all containers and volumes"
        exit 1
        ;;
esac
