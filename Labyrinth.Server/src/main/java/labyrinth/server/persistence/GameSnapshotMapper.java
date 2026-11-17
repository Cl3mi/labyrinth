package labyrinth.server.persistence;

import labyrinth.server.game.models.Board;
import labyrinth.server.game.models.Game;
import labyrinth.server.game.models.Player;
import labyrinth.server.game.models.PushTwiceActive;
import labyrinth.server.game.models.Tile;
import labyrinth.server.game.models.TreasureCard;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
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
}
