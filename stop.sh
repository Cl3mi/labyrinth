#!/bin/bash

# Labyrinth Stop Script
# Cleanly stops all server and client processes

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Stopping Labyrinth server and client...${NC}"
echo ""

STOPPED_SOMETHING=false

# Stop server on port 8081
if lsof -ti:8081 >/dev/null 2>&1; then
    echo -e "  → Stopping server on port 8081"
    lsof -ti:8081 | xargs kill -9 2>/dev/null || true
    STOPPED_SOMETHING=true
fi

# Stop client processes
if pgrep -f "exec:java" >/dev/null; then
    echo -e "  → Stopping client processes"
    pkill -9 -f "exec:java" || true
    STOPPED_SOMETHING=true
fi

# Stop any Spring Boot processes
if pgrep -f "spring-boot:run" >/dev/null; then
    echo -e "  → Stopping Spring Boot processes"
    pkill -9 -f "spring-boot:run" || true
    STOPPED_SOMETHING=true
fi

if [ "$STOPPED_SOMETHING" = true ]; then
    sleep 1
    echo ""
    echo -e "${GREEN}✓ All Labyrinth processes stopped${NC}"
else
    echo -e "${YELLOW}No running Labyrinth processes found${NC}"
fi

echo ""
echo -e "Server logs: ${YELLOW}/tmp/labyrinth-server.log${NC}"
echo ""
