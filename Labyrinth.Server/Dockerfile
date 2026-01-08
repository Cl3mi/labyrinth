# Multi-stage build for Labyrinth Server
# Stage 1: Build
FROM maven:3.9-eclipse-temurin-24 AS builder

WORKDIR /app

# Copy Contracts module first (for better caching)
COPY Labyrinth.Contracts/pom.xml Labyrinth.Contracts/pom.xml
COPY Labyrinth.Contracts/src Labyrinth.Contracts/src

# Build and install Contracts to local Maven repo
WORKDIR /app/Labyrinth.Contracts
RUN mvn clean install -DskipTests

# Copy Server module
WORKDIR /app
COPY Labyrinth.Server/pom.xml Labyrinth.Server/pom.xml
COPY Labyrinth.Server/src Labyrinth.Server/src

# Build Server
WORKDIR /app/Labyrinth.Server
RUN mvn clean package -DskipTests

# Stage 2: Runtime
FROM eclipse-temurin:24-jre-alpine

WORKDIR /app

# Copy the built jar from builder stage
COPY --from=builder /app/Labyrinth.Server/target/*.jar app.jar

# Expose the WebSocket port
EXPOSE 8082

# Run the application
ENTRYPOINT ["java", "-jar", "app.jar"]
