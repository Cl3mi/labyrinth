# Labyrinth

This project is a digital implementation of the Labyrinth board game. It includes a server, a desktop client, and a shared contracts module.

## Tech Stack

The project is built using the following technologies:

*   **Backend**:
    *   Java 24
    *   Spring Boot 3
    *   Maven
*   **Frontend (Desktop Client)**:
    *   Java 24
    *   Java Swing for the GUI
*   **Shared Contracts**:
    *   A plain Java library module for shared data models and interfaces between the client and server.
*   **Containerization**:
    *   Docker
    *   Docker Compose

## Project Structure

The project is a multi-module Maven project with the following components:

*   `Labyrinth.Contracts`: A shared library containing the data transfer objects (DTOs) and service interfaces used by both the client and server.
*   `Labyrinth.Server`: A Spring Boot application that functions as the game server, handling game logic and communication with clients via WebSockets.
*   `Labyrinth.Client`: A desktop application built with Java Swing that serves as the game's user interface.

## Technical Workflow

This section explains how to set up, build, test, and run the project.

### Prerequisites

*   Java JDK 24
*   Apache Maven
*   Docker and Docker Compose

### Setup and Build

The project uses Maven for dependency management and building.

*   **Local Build**: To build all modules locally, run the following command from the project root directory:
    ```bash
    mvn clean install
    ```
    This will compile the code, run tests, and package the applications into JAR files.

*   **Docker Build**: To build the Docker images for the server and tests, use the `Makefile`:
    ```bash
    make docker-build
    ```
    This command builds the necessary Docker images, including running the Maven build inside the containers.

### Testing

*   **Local Tests**: To run the tests locally using Maven:
    ```bash
    mvn test
    ```

*   **Docker Tests**: To run the tests in a consistent Docker environment:
    ```bash
    make docker-test
    ```
    This command builds the test image and runs the tests inside the container. The test results will be displayed in your terminal.

### Running the Application

*   **Running the Server**: To start the server, use Docker Compose:
    ```bash
    make docker-up
    ```
    This will start the Labyrinth server, which will be accessible on `http://localhost:8080`.

*   **Running the Client**: The client is a desktop application and should be run on your local machine.
    1.  Make sure you have built the project with `mvn clean install`.
    2.  Navigate to the client's `target` directory:
        ```bash
        cd Labyrinth.Client/target
        ```
    3.  Run the executable JAR file:
        ```bash
        java -jar labyrinth-client-1.0.0-jar-with-dependencies.jar
        ```
    This will launch the game's GUI.

---

## Original README Content

# labyrinth
Advanced Integrative Project MCI Master Digital Business and Software Engineering

# Labyrinth.Game
Folder to play around with the game logic. Adjust values like board size in testing class.
Start the Testing main() to simulate a game startup with a simple debug UI.
Press "R" to load a new game.

So far following gets visualized/are implemented:
- Players
- MxN Board size with proper fixed tiles
- All reachable tiles with green background
- All unreachable tiles with dark grey background
- Corridors
- Shift corridors with arrow, for now all (even fix) tiles are shiftable and players do not get shiftet. But the reachable tiles graph updates.
- Treasures on a tile
- Press "p" to "switch" player
