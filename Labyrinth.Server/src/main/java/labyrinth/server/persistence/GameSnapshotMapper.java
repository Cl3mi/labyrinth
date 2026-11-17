package labyrinth.server.persistence;

import labyrinth.server.game.enums.BonusTypes;
import labyrinth.server.game.enums.MoveState;
import labyrinth.server.game.models.ActiveBonus;
import labyrinth.server.game.models.BiMap;
import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.BonusState;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.NoBonusActive;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.PlayerStatistics;
import labyrinth.server.game.models.PushTwiceActive;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.models.TreasureCard;
import labyrinth.server.game.models.records.GameConfig;
import labyrinth.server.game.models.records.LastShift;
import labyrinth.server.game.models.records.Position;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;

public final class GameSnapshotMapper {

    private GameSnapshotMapper() {
    }

    public static GameSnapshot capture(UUID gameId, long version, Game game,
                                       Map<UUID, UUID> tokensByPlayerId, OffsetDateTime now) {
        var board = game.getBoard();
        var config = game.getGameConfig();
        var treasures = new TreeMap<Integer, GameSnapshot.Treasure>();

        var players = new ArrayList<GameSnapshot.PlayerState>();
        for (Player p : game.getPlayers()) {
            var treasureIds = new ArrayList<Integer>();
            for (TreasureCard card : p.getAssignedTreasureCards()) {
                treasureIds.add(addTreasure(treasures, card));
            }
            var stats = p.getStatistics();
            players.add(new GameSnapshot.PlayerState(
                    p.getId(),
                    p.getUsername(),
                    tokensByPlayerId.get(p.getId()),
                    p.getColor(),
                    p.getJoinDate(),
                    p.isAdmin(),
                    p.isAiActive(),
                    p.isDisconnected(),
                    positionOf(board, p.getCurrentTile()),
                    positionOf(board, p.getHomeTile()),
                    List.copyOf(treasureIds),
                    List.copyOf(p.getBonuses()),
                    new GameSnapshot.Statistics(
                            stats.getScore(),
                            stats.getStepsTaken(),
                            stats.getTilesPushed(),
                            stats.getTreasuresCollected(),
                            List.copyOf(stats.getCollectedAchievements()))));
        }

        var rows = new ArrayList<List<GameSnapshot.TileState>>();
        for (int row = 0; row < board.getHeight(); row++) {
            var tiles = new ArrayList<GameSnapshot.TileState>();
            for (int col = 0; col < board.getWidth(); col++) {
                tiles.add(tile(board.getTileAt(row, col), treasures));
            }
            rows.add(List.copyOf(tiles));
        }
        var boardState = new GameSnapshot.BoardState(
                board.getWidth(),
                board.getHeight(),
                board.isFreeRoam(),
                List.copyOf(rows),
                tile(board.getExtraTile(), treasures));

        var turn = new GameSnapshot.TurnState(
                game.getCurrentPlayerIndex(),
                game.getCurrentMoveState(),
                game.isBonusUsedThisTurn(),
                game.getLastShift()
                        .map(s -> new GameSnapshot.Shift(s.index(), s.direction()))
                        .orElse(null));

        var bonusState = game.getActiveBonusState();
        GameSnapshot.BonusState activeBonus = bonusState.getBonusType()
                .map(type -> new GameSnapshot.BonusState(type,
                        bonusState instanceof PushTwiceActive p ? p.getRemainingPushes() : 0))
                .orElse(null);

        return new GameSnapshot(
                GameSnapshot.FORMAT_VERSION,
                gameId,
                version,
                now,
                new GameSnapshot.Config(
                        config.boardWidth(),
                        config.boardHeight(),
                        config.treasureCardCount(),
                        config.gameDurationInSeconds(),
                        config.totalBonusCount(),
                        config.turnTimeInSeconds()),
                game.getGameEndTime(),
                game.getTurnEndTime(),
                boardState,
                List.copyOf(treasures.values()),
                List.copyOf(players),
                turn,
                activeBonus);
    }

    private static GameSnapshot.TileState tile(Tile tile, Map<Integer, GameSnapshot.Treasure> treasures) {
        var card = tile.getTreasureCard();
        return new GameSnapshot.TileState(
                Collections.unmodifiableSet(tile.getEntrances()),
                tile.isFixed(),
                card == null ? null : addTreasure(treasures, card),
                tile.getBonus());
    }

    private static int addTreasure(Map<Integer, GameSnapshot.Treasure> treasures, TreasureCard card) {
        treasures.put(card.getId(), new GameSnapshot.Treasure(card.getId(), card.getTreasureName(), card.isCollected()));
        return card.getId();
    }

    private static GameSnapshot.Position positionOf(Board board, Tile tile) {
        if (tile == null) {
            return null;
        }
        var pos = board.getPositionOfTile(tile);
        return pos == null ? null : new GameSnapshot.Position(pos.row(), pos.column());
    }

    /** Domain objects rebuilt from a snapshot, ready for {@link Game#restore}. */
    public record RestoredGame(
            GameConfig config,
            Board board,
            List<Player> players,
            OffsetDateTime gameStartTime,
            BonusState activeBonus,
            int currentPlayerIndex,
            MoveState moveState,
            boolean bonusUsedThisTurn,
            LastShift lastShift,
            Map<UUID, UUID> tokensByPlayerId
    ) {
    }

    public static RestoredGame restore(GameSnapshot s) {
        if (s.formatVersion() != GameSnapshot.FORMAT_VERSION) {
            throw new IllegalArgumentException("Unsupported snapshot format " + s.formatVersion());
        }
        var c = s.config();
        var config = new GameConfig(c.boardWidth(), c.boardHeight(), c.treasureCardCount(),
                c.gameDurationInSeconds(), c.totalBonusCount(), c.turnTimeInSeconds());

        // one instance per treasure, shared by the tile and the player who has to find it
        var cards = new HashMap<Integer, TreasureCard>();
        for (var t : s.treasures()) {
            var card = new TreasureCard(t.id(), t.name());
            if (t.collected()) {
                card.collect();
            }
            cards.put(t.id(), card);
        }

        var b = s.board();
        if (b.rows().size() != b.height()) {
            throw new IllegalArgumentException("Board has " + b.rows().size() + " rows, expected " + b.height());
        }
        var tiles = new BiMap<Position, Tile>();
        for (int row = 0; row < b.height(); row++) {
            var cols = b.rows().get(row);
            if (cols.size() != b.width()) {
                throw new IllegalArgumentException("Row " + row + " has " + cols.size() + " tiles, expected " + b.width());
            }
            for (int col = 0; col < b.width(); col++) {
                tiles.put(new Position(row, col), tile(cols.get(col), cards));
            }
        }
        var board = new Board(b.width(), b.height(), tiles, tile(b.extraTile(), cards));
        board.setFreeRoam(b.freeRoam());

        var players = new ArrayList<Player>();
        var tokens = new HashMap<UUID, UUID>();
        for (var ps : s.players()) {
            var player = new Player(ps.id(), ps.username());
            player.setColor(ps.color());
            player.setJoinDate(ps.joinDate());
            player.setAdmin(ps.admin());
            player.setAiActive(ps.aiActive());
            player.setDisconnected(ps.disconnected());
            player.setCurrentTile(tileAt(board, ps.position(), ps.username()));
            player.setHomeTile(tileAt(board, ps.home(), ps.username()));
            for (int id : ps.treasureIds()) {
                var card = cards.get(id);
                if (card == null) {
                    throw new IllegalArgumentException("Unknown treasure " + id + " for " + ps.username());
                }
                player.getAssignedTreasureCards().add(card);
            }
            player.getBonuses().addAll(ps.bonuses());
            player.setStatistics(statistics(ps.statistics()));
            players.add(player);
            if (ps.identifierToken() != null) {
                tokens.put(ps.id(), ps.identifierToken());
            }
        }

        var turn = s.turn();
        var lastShift = turn.lastShift() == null ? null
                : new LastShift(turn.lastShift().index(), turn.lastShift().direction());

        return new RestoredGame(
                config,
                board,
                List.copyOf(players),
                s.gameEndTime().minusSeconds(config.gameDurationInSeconds()),
                bonusState(s.activeBonus()),
                turn.currentPlayerIndex(),
                turn.moveState(),
                turn.bonusUsedThisTurn(),
                lastShift,
                Map.copyOf(tokens));
    }

    private static Tile tile(GameSnapshot.TileState t, Map<Integer, TreasureCard> cards) {
        var tile = new Tile(t.entrances());
        tile.setIsFixed(t.fixed());
        tile.setBonus(t.bonus());
        if (t.treasureId() != null) {
            var card = cards.get(t.treasureId());
            if (card == null) {
                throw new IllegalArgumentException("Unknown treasure " + t.treasureId() + " on tile");
            }
            tile.setTreasureCard(card);
        }
        return tile;
    }

    private static Tile tileAt(Board board, GameSnapshot.Position p, String username) {
        var tile = p == null ? null : board.getTileAt(p.row(), p.column());
        if (tile == null) {
            throw new IllegalArgumentException("Player " + username + " is not on the board");
        }
        return tile;
    }

    private static PlayerStatistics statistics(GameSnapshot.Statistics s) {
        var stats = new PlayerStatistics();
        stats.increaseScore(s.score());
        stats.increaseStepsTaken(s.stepsTaken());
        stats.increaseTilesPushed(s.tilesPushed());
        stats.increaseTreasuresCollected(s.treasuresCollected());
        s.achievements().forEach(stats::collectAchievement);
        return stats;
    }

    private static BonusState bonusState(GameSnapshot.BonusState b) {
        if (b == null) {
            return NoBonusActive.getInstance();
        }
        if (b.type() == BonusTypes.PUSH_TWICE) {
            return PushTwiceActive.withRemainingPushes(b.remainingPushes());
        }
        return new ActiveBonus(b.type());
    }
}
