import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Player {
    private final String name;
    private final List<Card> hand;
    public Player next;
    public Player prev;


    public Player(String name) {
        this.name = name;
        this.hand = new ArrayList<>();
    }

    public String getName() {
        return name;
    }

    public List<Card> getHand() {
        return hand; // seguridad
    }

    public void receiveCard(Card card) {
        hand.add(card);
    }

    public boolean hasWon() {
        return hand.isEmpty();
    }

    public Card playCard(Card topCard) {
        for (Card card : hand) {
            if (card.PlayOn(topCard) ){
                hand.remove(card);
                return card;
            }
        }
        return null; // No se puede jugar
    }

    public boolean canPlay(Card topCard) {
        return hand.stream().anyMatch(card -> card.PlayOn(topCard));
    }
}
