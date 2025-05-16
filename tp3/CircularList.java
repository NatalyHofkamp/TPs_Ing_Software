import java.util.List;

public class CircularList {

    private Player current;
    private boolean reverse = false;

    public CircularList(List<Player> players) {
        if (players.isEmpty()) throw new IllegalArgumentException("Lista vacía");

        Player first = players.get(0);
        Player prev = first;

        for (int i = 1; i < players.size(); i++) {
            Player node = players.get(i);
            prev.next = node;
            node.prev = prev;
            prev = node;
        }

        // cerrar el ciclo
        prev.next = first;
        first.prev = prev;

        current = first;
    }

    public Player getCurrent() {
        return current;
    }

    public void next() {
        current = reverse ? current.prev : current.next;
    }

    public void toggleDirection() {
        reverse = !reverse;
    }

    public boolean isReverse() {
        return reverse;
    }

    public int size() {
        int count = 1;
        Player start = current;
        Player node = current.next;
        while (node != start) {
            count++;
            node = node.next;
        }
        return count;
    }
}

