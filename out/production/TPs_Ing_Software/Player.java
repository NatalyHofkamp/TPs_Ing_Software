import java.util.ArrayList;
import java.util.List;

public class Player {
    private final String name;
    private final List<Carta> hand;
    public Player next;
    public Player prev;


    public Player(String name) {
        this.name = name;
        this.hand = new ArrayList<>();
    }

    public String getName() {
        return name;
    }

    public List<Carta> getHand() {
        return hand; // seguridad
    }

    public void receiveCard(Carta card) {
        hand.add(card);
    }

    public boolean hasWon() {
        return hand.isEmpty();
    }

    public Carta playCard(Carta topCard) {
        for (Carta card : hand) {
            if (card.PlayOn(topCard) ){
                hand.remove(card);
                return card;
            }
        }
        return null; // No se puede jugar
    }

    public boolean canPlay(Carta topCard) {
        return hand.stream().anyMatch(card -> card.PlayOn(topCard));
    }
}
