package labyrinth.server.persistence;

import org.springframework.jdbc.datasource.DriverManagerDataSource;

class H2GameRecorderTest extends AbstractGameRecorderTest {

    @Override
    protected DriverManagerDataSource dataSource() {
        // Same URL style as the default in application.properties
        return new DriverManagerDataSource(
                "jdbc:h2:mem:recorder-test;MODE=PostgreSQL;DATABASE_TO_LOWER=TRUE;DB_CLOSE_DELAY=-1", "sa", "");
    }
}
