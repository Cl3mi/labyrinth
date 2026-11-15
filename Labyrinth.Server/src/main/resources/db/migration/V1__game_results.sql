-- Games played on any server instance. All instances write into the same database,
-- but each instance only writes rows for its own games.
CREATE TABLE game (
    id                  UUID PRIMARY KEY,
    instance_id         VARCHAR(200) NOT NULL,
    server_name         VARCHAR(200) NOT NULL,
    status              VARCHAR(20)  NOT NULL,  -- IN_GAME, FINISHED, ABORTED
    started_at          TIMESTAMP WITH TIME ZONE NOT NULL,
    ended_at            TIMESTAMP WITH TIME ZONE,
    board_width         INTEGER NOT NULL,
    board_height        INTEGER NOT NULL,
    treasure_card_count INTEGER NOT NULL,
    bonus_count         INTEGER NOT NULL,
    turn_time_s         INTEGER NOT NULL,
    game_duration_s     INTEGER NOT NULL,
    player_count        INTEGER NOT NULL
);

CREATE INDEX idx_game_status ON game (status);
CREATE INDEX idx_game_instance ON game (instance_id);

-- Final ranking of a finished game, one row per player (humans and bots).
CREATE TABLE player_result (
    game_id             UUID         NOT NULL REFERENCES game (id),
    player_id           UUID         NOT NULL,
    username            VARCHAR(100) NOT NULL,
    is_bot              BOOLEAN      NOT NULL,
    placement           INTEGER      NOT NULL,
    score               INTEGER      NOT NULL,
    treasures_collected INTEGER      NOT NULL,
    steps_taken         INTEGER      NOT NULL,
    tiles_pushed        INTEGER      NOT NULL,
    achievements        VARCHAR(500) NOT NULL,  -- comma separated, may be empty
    ended_game          BOOLEAN      NOT NULL,  -- player who reached home and ended the game
    PRIMARY KEY (game_id, player_id)
);
