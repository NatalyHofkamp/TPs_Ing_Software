import java.util.*;

public class Deck {
    private final Deque<Card> cartas = new ArrayDeque<>();;

    public void addCard(Card card) {
        cartas.push(card);
    }

    public Card getCard() {
        if (cartas.isEmpty()) {
            throw new IllegalStateException("El mazo está vacío");
        }
        return cartas.pop();
    }

    public Card verCartaSuperior() {
        return cartas.peek(); // No la remueve
    }

    public boolean estaVacio() {
        return cartas.isEmpty();
    }

    public int cantidad() {
        return cartas.size();
    }


    public void shuffle() {
        List<Card> lista = new ArrayList<>(cartas);
        Collections.shuffle(lista); // No usar en tests deterministas
        cartas.clear();
        cartas.addAll(lista);
    }
}
