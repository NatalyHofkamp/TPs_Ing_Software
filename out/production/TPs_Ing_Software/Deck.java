import java.util.*;

public class Deck {
    private final Deque<Carta> cartas = new ArrayDeque<>();;

    public void addCard(Carta card) {
        cartas.push(card);
    }

    public Carta getCard() {
        if (cartas.isEmpty()) {
            throw new IllegalStateException("El mazo está vacío");
        }
        return cartas.pop();
    }

    public Carta verCartaSuperior() {
        return cartas.peek(); // No la remueve
    }

    public boolean estaVacio() {
        return cartas.isEmpty();
    }

    public int cantidad() {
        return cartas.size();
    }


    public void shuffle() {
        List<Carta> lista = new ArrayList<>(cartas);
        Collections.shuffle(lista); // No usar en tests deterministas
        cartas.clear();
        cartas.addAll(lista);
    }
}
