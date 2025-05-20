import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

public class GameUNO {
    private CircularList<Player> players;
    private Deque<Carta> Pit = new LinkedList<>();
    private Deque<Carta> drawPile = new LinkedList<>();

    public GameUNO(List<Player> playerList, Deque<Carta> Pit, Deque<Carta> drawPile) {
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
            Carta drawnCard = drawPile.pop();
            currentPlayer.receiveCard(drawnCard);
        }
    }
    public Player getCurrentPlayer() {
        return players.getCurrent();
    }
    public Carta topCard() {
        return Pit.peek(); // Devuelve la carta en la cima sin removerla
    }

    public void playTurn(Carta playedCard) {
        Carta topCard = Pit.peek();
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

