# Top-level helper Makefile
.PHONY: build test docker-build docker-up docker-test

# Build all Labyrinth.* modules (use your desired Maven options)
build:
	mvn -pl 'Labyrinth.*' -am -T 1C -B -DskipTests package

# Run all tests across Labyrinth.* modules
test:
	mvn -pl 'Labyrinth.*' -am -B test

# Build docker images using docker-compose (runs local Dockerfile builds)
docker-build:
	docker-compose build

# Build images (via compose) and bring up services
docker-up: docker-build
	docker-compose up -d

# Run tests in docker
docker-test:
	docker-compose build test
