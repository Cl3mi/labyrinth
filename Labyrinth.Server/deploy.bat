@echo off
setlocal enabledelayedexpansion

:: --- CONFIGURATION ---
set LOCAL_JAR=target/labyrinth-server-1.0.0.jar
set SERVER_USER=sudash
set SERVER_IP=130.61.83.237
set REMOTE_PATH=/home/sudash/apps/lab
set PROP_FILE=src\main\resources\application.properties
:: ---------------------

echo [1/5] Modifying application.properties for Server...
:: Backup original
copy "%PROP_FILE%" "%PROP_FILE%.bak" >nul

:: Create temporary server version
echo spring.application.name=Labyrinth.Server> "%PROP_FILE%"
echo server.name=Group1>> "%PROP_FILE%"
echo server.port=8082>> "%PROP_FILE%"
echo server.wsendpoint=/game>> "%PROP_FILE%"
echo server.publichost=%SERVER_IP%>> "%PROP_FILE%"
echo management.api.url=https://mgmt.dvl.spalx.dev>> "%PROP_FILE%"

echo [2/5] Building JAR with Maven (Java 25)...
call ./mvnw clean package

:: Restore original file immediately after build
move /y "%PROP_FILE%.bak" "%PROP_FILE%" >nul

if %ERRORLEVEL% NEQ 0 (
    echo [ERROR] Maven build failed!
    pause
    exit /b %ERRORLEVEL%
)

echo [3/5] Uploading JAR to server...
scp "%LOCAL_JAR%" %SERVER_USER%@%SERVER_IP%:%REMOTE_PATH%/app.jar

echo [4/5] Rebuilding and restarting Docker containers...
ssh %SERVER_USER%@%SERVER_IP% "cd %REMOTE_PATH% && docker compose up -d --build"

echo [5/5] Cleaning up old Docker images...
ssh %SERVER_USER%@%SERVER_IP% "docker image prune -f"

echo.
echo ===========================================
echo   DEPLOYMENT SUCCESSFUL!
echo   App IP updated to: %SERVER_IP%
echo   Local properties restored to: localhost
echo ===========================================
pause