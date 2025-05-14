import java.util.List;

public class CircularList<T> {
    private static class Node<T> {
        T value;
        Node<T> next;
        Node<T> prev;

        Node(T value) {
            this.value = value;
        }
    }

    private Node<T> current;
    private boolean reverse = false;

    public CircularList(List<T> items) {
        if (items.isEmpty()) throw new IllegalArgumentException("Lista vacía");

        Node<T> first = new Node<>(items.get(0));
        Node<T> prev = first;

        for (int i = 1; i < items.size(); i++) {
            Node<T> node = new Node<>(items.get(i));
            prev.next = node;
            node.prev = prev;
            prev = node;
        }

        // cerrar el ciclo
        prev.next = first;
        first.prev = prev;

        current = first;
    }

    public T getCurrent() {
        return current.value;
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
        // opcional: para usar si querés saber cuántos hay
        int count = 1;
        Node<T> start = current;
        Node<T> node = current.next;
        while (node != start) {
            count++;
            node = node.next;
        }
        return count;
    }
}
