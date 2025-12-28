#!/bin/bash

# Labyrinth Clean Restart Script
# Usage:
#   ./restart.sh           - Full clean restart (rebuild + restart)
#   ./restart.sh --quick   - Quick restart (no rebuild)
#   ./restart.sh --server  - Only restart server
#   ./restart.sh --client  - Only restart client

set -e  # Exit on error

PROJECT_ROOT="/home/clemi/sites/labyrinth"
SERVER_DIR="$PROJECT_ROOT/Labyrinth.Server"
CLIENT_DIR="$PROJECT_ROOT/Labyrinth.Client"
CONTRACTS_DIR="$PROJECT_ROOT/Labyrinth.Contracts"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Parse arguments
REBUILD=true
START_SERVER=true
START_CLIENT=true

for arg in "$@"; do
    case $arg in
        --quick)
            REBUILD=false
            ;;
        --server)
            START_CLIENT=false
            ;;
        --client)
            START_SERVER=false
            REBUILD=false
            ;;
    esac
done

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  Labyrinth Clean Restart Script${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Step 1: Kill existing processes
echo -e "${YELLOW}[1/6] Killing existing server and client processes...${NC}"

# Kill server processes on port 8081
if lsof -ti:8081 >/dev/null 2>&1; then
    echo "  → Killing process on port 8081"
    lsof -ti:8081 | xargs kill -9 2>/dev/null || true
    sleep 1
fi

# Kill any Maven exec:java processes (client)
if pgrep -f "exec:java" >/dev/null; then
    echo "  → Killing client processes"
    pkill -9 -f "exec:java" || true
    sleep 1
fi

# Kill any Spring Boot processes
if pgrep -f "spring-boot:run" >/dev/null; then
    echo "  → Killing Spring Boot processes"
    pkill -9 -f "spring-boot:run" || true
    sleep 1
fi

echo -e "${GREEN}  ✓ All processes terminated${NC}"
echo ""

# Step 2: Clear client stored tokens
echo -e "${YELLOW}[2/6] Clearing client stored tokens and preferences...${NC}"
rm -rf ~/.java/.userPrefs/labyrinth/ 2>/dev/null || true
echo -e "${GREEN}  ✓ Client preferences cleared${NC}"
echo ""

# Step 3: Rebuild (if not quick mode)
if [ "$REBUILD" = true ]; then
    echo -e "${YELLOW}[3/6] Rebuilding modules (Contracts → Server → Client)...${NC}"

    echo "  → Building Contracts..."
    cd "$CONTRACTS_DIR"
    rm -rf target/
    mvn clean install -q -Dmaven.test.skip=true

    if [ "$START_SERVER" = true ]; then
        echo "  → Building Server..."
        cd "$SERVER_DIR"
        mvn clean package -q -Dmaven.test.skip=true
    fi

    if [ "$START_CLIENT" = true ]; then
        echo "  → Building Client..."
        cd "$CLIENT_DIR"
        mvn clean package -q -Dmaven.test.skip=true
    fi

    echo -e "${GREEN}  ✓ Build completed${NC}"
else
    echo -e "${YELLOW}[3/6] Skipping rebuild (quick mode)${NC}"
fi
echo ""

# Step 4: Start server
if [ "$START_SERVER" = true ]; then
    echo -e "${YELLOW}[4/6] Starting server on port 8081...${NC}"
    cd "$SERVER_DIR"

    # Start server in background (skip tests to avoid compilation errors)
    mvn spring-boot:run -Dmaven.test.skip=true > /tmp/labyrinth-server.log 2>&1 &
    SERVER_PID=$!

    echo "  → Server PID: $SERVER_PID"
    echo "  → Waiting for server to start..."

    # Wait for server to be ready (max 30 seconds)
    COUNTER=0
    while [ $COUNTER -lt 30 ]; do
        if lsof -ti:8081 >/dev/null 2>&1; then
            echo -e "${GREEN}  ✓ Server is ready!${NC}"
            break
        fi
        sleep 1
        COUNTER=$((COUNTER + 1))
        echo -n "."
    done
    echo ""

    if [ $COUNTER -eq 30 ]; then
        echo -e "${RED}  ✗ Server failed to start within 30 seconds${NC}"
        echo -e "${RED}  Check logs: tail -f /tmp/labyrinth-server.log${NC}"
        exit 1
    fi

    echo -e "${GREEN}  Server is running on http://localhost:8081${NC}"
    echo -e "${BLUE}  Logs: tail -f /tmp/labyrinth-server.log${NC}"
else
    echo -e "${YELLOW}[4/6] Skipping server start${NC}"
fi
echo ""

# Step 5: Wait a moment for server to fully initialize
if [ "$START_SERVER" = true ]; then
    echo -e "${YELLOW}[5/6] Waiting for server to fully initialize...${NC}"
    sleep 3
    echo -e "${GREEN}  ✓ Server ready${NC}"
else
    echo -e "${YELLOW}[5/6] Skipping initialization wait${NC}"
fi
echo ""

# Step 6: Start client
if [ "$START_CLIENT" = true ]; then
    echo -e "${YELLOW}[6/6] Starting client...${NC}"
    cd "$CLIENT_DIR"

    echo -e "${GREEN}  ✓ Client starting in new window${NC}"
    echo ""

    # Start client (will run in foreground, shows GUI)
    mvn exec:java
else
    echo -e "${YELLOW}[6/6] Skipping client start${NC}"
    echo ""
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN}  Server is ready!${NC}"
    echo -e "${GREEN}========================================${NC}"
    echo ""
    echo -e "To start client manually, run:"
    echo -e "${BLUE}  cd $CLIENT_DIR && mvn exec:java${NC}"
    echo ""
    echo -e "To stop server:"
    echo -e "${BLUE}  kill $(lsof -ti:8081 2>/dev/null || echo 'N/A')${NC}"
    echo ""
    echo -e "To view server logs:"
    echo -e "${BLUE}  tail -f /tmp/labyrinth-server.log${NC}"
    echo ""
fi
