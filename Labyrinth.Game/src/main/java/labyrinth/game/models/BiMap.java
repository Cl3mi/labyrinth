package labyrinth.game.models;

import java.util.HashMap;
import java.util.Map;

public class BiMap<K, V> {
    private final Map<K, V> forward = new HashMap<>();
    private final Map<V, K> backward = new HashMap<>();

    public void put(K key, V value) {
        if (forward.containsKey(key)) {
            backward.remove(forward.get(key));
        }
        if (backward.containsKey(value)) {
            forward.remove(backward.get(value));
        }
        forward.put(key, value);
        backward.put(value, key);
    }

    public V getForward(K key) {
        return forward.get(key);
    }

    public K getBackward(V value) {
        return backward.get(value);
    }

    public void removeByKey(K key) {
        V value = forward.remove(key);
        if (value != null) backward.remove(value);
    }

    public void removeByValue(V value) {
        K key = backward.remove(value);
        if (key != null) forward.remove(key);
    }
}