ARG JAVA_VERSION=24

# Builder stage (INFO: i used an OpenJDK image)
FROM eclipse-temurin:${JAVA_VERSION} AS builder

ARG MAVEN_VERSION=3.9.6

# manually installing Maven like in the other Dockerfiles
RUN apt-get update && apt-get install -y --no-install-recommends curl tar ca-certificates \
  && curl -fsSL https://archive.apache.org/dist/maven/maven-3/${MAVEN_VERSION}/binaries/apache-maven-${MAVEN_VERSION}-bin.tar.gz -o /tmp/maven.tar.gz \
  && tar -xzf /tmp/maven.tar.gz -C /opt \
  && mv /opt/apache-maven-${MAVEN_VERSION} /opt/maven \
  && ln -s /opt/maven/bin/mvn /usr/bin/mvn \
  && rm -rf /var/lib/apt/lists/* /tmp/maven.tar.gz

WORKDIR /build
COPY . /build

RUN mvn -B -DskipTests package

# Runtime stage
FROM eclipse-temurin:${JAVA_VERSION}

WORKDIR /app
COPY --from=builder /build/Labyrinth.Server/target/*.jar /app/app.jar

EXPOSE 8080

ENTRYPOINT ["java", "-jar", "/app/app.jar"]
