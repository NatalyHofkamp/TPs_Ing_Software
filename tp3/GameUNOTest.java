import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

public class GameUNOTest {

    private GameUNO game;
    private Player player1;
    private Player player2;
    private Card red5;
    private Card red7;

    @BeforeEach
    public void setup() {
        // Crear jugadores
        player1 = new Player("Luidolacrack");
        player2 = new Player("Bob");

        // Crear mazo vacío para test
        LinkedList<Card> discardPile = new LinkedList<>();

        // Crear pila de descarte con una carta roja 5
        Deque<Card> drawPile = new LinkedList<>();
        red5 = new NumberCard("Red", 5);
        drawPile.push(red5);

        // Crear juego con 2 jugadores y pila de descarte
        List<Player> players = Arrays.asList(player1, player2);
        game = new GameUNO(players, discardPile, drawPile);

        // Darle al jugador 1 una carta roja 7 (jugable)
        red7 = new NumberCard("Red", 7);
        player1.receiveCard(red7);
    }

    @Test
    public void testJugadorJuegaCartaValida() {
        // Precondición: la carta top es roja 5
        assertEquals(red5, game.topCard());

        // Jugador 1 juega rojo 7
        game.playTurn(red7);

        // La carta jugada debería estar en la cima
        assertEquals(red7, game.topCard());

        // La carta debería haber sido removida de su mano
        assertFalse(player1.getHand().contains(red7));

        // Ahora es el turno de Bob (jugador 2)
        assertEquals(player2, game.getCurrentPlayer());
    }
}

