-- H2 (PostgreSQL mode) accepts JSONB as an alias for JSON
CREATE TABLE game_snapshot (
    game_id     UUID PRIMARY KEY REFERENCES game (id),
    version     BIGINT NOT NULL,
    updated_at  TIMESTAMP WITH TIME ZONE NOT NULL,
    state       JSONB NOT NULL
);
