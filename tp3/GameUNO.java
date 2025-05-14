import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

public class GameUNO {
    private CircularList<Player> players;
    private Deque<Card> Pit = new LinkedList<>();
    private Deque<Card> drawPile = new LinkedList<>();

    public GameUNO(List<Player> playerList, Deque<Card> Pit, Deque<Card> drawPile) {
        this.players = new CircularList<>(playerList);
        this.drawPile.addAll(Pit);
        this.Pit.add(drawPile.pop()); // primera carta
    }
    public void drawCards(int count) {
        Player currentPlayer = getCurrentPlayer();

        for (int i = 0; i < count; i++) {
            if (drawPile.isEmpty()) {
                // Si el mazo se vacía, reorganizamos el descarte
                return;
            }
            // Robamos una carta y la damos al jugador
            Card drawnCard = drawPile.pop();
            currentPlayer.receiveCard(drawnCard);
        }
    }
    public Player getCurrentPlayer() {
        return players.getCurrent();
    }
    public Card topCard() {
        return Pit.peek(); // Devuelve la carta en la cima sin removerla
    }

    public void playTurn(Card playedCard) {
        Card topCard = Pit.peek();
        if (players.getCurrent().playCard(topCard()) == null){
            drawCards(1);
        }
        else{
            Pit.push(playedCard);
            playedCard.applyEffect(this);
            players.next();
        }
    }

    public GameUNO reverseTurnOrder() {
        players.toggleDirection();
        return this;
    }

    public GameUNO skipNextPlayer() {
        players.next(); // salteás uno
        return this;
    }


}

