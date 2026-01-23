# Labyrinth

Labyrinth is a Java-based multiplayer labyrinth board game. The project is split into several Maven modules:

- `Labyrinth.Contracts` — shared DTOs, interfaces and models used by server and clients.
- `Labyrinth.ManagementClient` — management API client and helper libraries (OpenAPI-generated client code).
- `Labyrinth.Client` — Java client (UI / player client).
- `Labyrinth.Server` — backend (Spring Boot) hosting game instances, WebSocket communication and persistence.


This README provides a short project overview, minimal build instructions, and how to start the server using provided artifacts.

## Short game description

Labyrinth is a tile-based board game with shiftable tiles, players and treasures. The server manages game state, connects clients via WebSocket/HTTP and exposes management APIs. Clients render the game and send player actions to the server.

## Important notes

- The modules are Maven projects; some modules generate sources (OpenAPI) and some artifacts must be installed into the local Maven repository (for example `Labyrinth.Contracts`).
- Java version: the project sets `java.version` to 25 — verify your local JDK version.

## Quickstart — Build

1) Install `Contracts` into the local Maven repository (required by other modules):

```bash
cd Labyrinth.Contracts
mvn clean install
```

2) Build / install `ManagementClient` (may generate sources via OpenAPI):

```bash
cd ../Labyrinth.ManagementClient
mvn clean install
```

3) Optional: build the client (if you need a runnable client JAR):

```bash
cd ../Labyrinth.Client
mvn clean package
```

4) Build the server (if you want to create the server JAR yourself):

```bash
cd ../Labyrinth.Server
mvn clean package
# Result: target/labyrinth-server-<version>.jar
```


Note: If you already receive the two JARs and an `application.properties` from the team (see "Quick start with provided artifacts"), you do not need to rebuild everything locally.

## Quick start with provided artifacts

If you receive the server JARs and an `application.properties`, follow these steps:

1) Put the files into a folder, e.g. `run/`:

- `labyrinth-server.jar` (server fat JAR)
- `labyrinth-client.jar` (optional: client JAR if provided)
- `application.properties` (server configuration)

2) Start the server (example):

If the `application.properties` file is in the same folder as the JAR you can start the server without extra arguments:

```bash
cd run
java -jar labyrinth-server.jar
```

Alternatively, you may explicitly point to the config file:

```bash
java -jar labyrinth-server.jar --spring.config.location=./application.properties
```

Or with a system property:

```bash
java -Dspring.config.location=./application.properties -jar labyrinth-server.jar
```


The server will read the `application.properties` from the current folder or from the supplied location. The repository contains a sample `application.properties` with port and other defaults (see below).

## Important configuration parameters (example)

The example `application.properties` in the repository contains:

```
spring.application.name=Labyrinth.Server

server.name=Group 1
server.port=8082
server.publichost=localhost
management.api.url=https://mgmt.dvl.spalx.dev
```

- `server.port` — port the server listens on (default in sample: `8082`).
- `server.publichost` — public hostname for clients to connect to.
- `management.api.url` — (optional) URL of an external management API.

Adjust these values in the provided `application.properties` if required.

## Troubleshooting

- Missing dependencies on startup: ensure `Labyrinth.Contracts` and `Labyrinth.ManagementClient` were installed locally (`mvn clean install`).
- Port already in use: change `server.port` in `application.properties` or start the server with `-Dserver.port=XXXX`.
- Java version: project is configured for Java 25 — ensure your JDK is compatible.







