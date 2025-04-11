import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

private void initializeDeck() {
    // Add Numbered Cards (0-9 for each color, twice for numbers 1-9)
    for (CardColor color : CardColor.values()) {
        if (color == CardColor.BLACK) continue; // Black is only for Wild cards
        Mazo.add(new NumberedCard(color, 0));
        for (int i = 1; i <= 9; i++) {
            Mazo.add(new NumberedCard(color, i));
            Mazo.add(new NumberedCard(color, i));
        }

        // Special Cards for each color
        Mazo.add(new DrawTwo(color));
        Mazo.add(new DrawTwo(color));
        Mazo.add(new Reverse(color));
        Mazo.add(new Reverse(color));
        Mazo.add(new Skip(color));
        Mazo.add(new Skip(color));
    }

    // Add Wild Cards
    for (int i = 0; i < 4; i++) {
        Mazo.add(new Wild());
    }
}
}


@DisplayName("UNO Game Logic Tests")
public class GameLogicTest {

    @Test
    @DisplayName("1. El juego inicia con un número válido de jugadores (>=2)")
    void gameStartsWithValidPlayers() {
        // Setup del juego con 2 jugadores
        // Verifica que el juego se inicialice correctamente
    }

    @Test
    @DisplayName("2. El jugador inicial es el primero en la lista")
    void firstPlayerStartsGame() {
        // Asegura que el turno inicial pertenece al primer jugador
    }

    @Test
    @DisplayName("3. Un jugador puede jugar una carta del mismo color o número")
    void playerCanPlayMatchingColorOrNumber() {
        // Verifica que una carta del mismo color o número sea aceptada como jugada válida
    }

    @Test
    @DisplayName("4. Se salta correctamente el turno con una carta Skip")
    void skipCardSkipsNextPlayer() {
        // Simula una carta Skip y verifica que el siguiente jugador pierde el turno
    }

    @Test
    @DisplayName("5. La dirección cambia con una carta Reverse")
    void reverseCardChangesDirection() {
        // Simula que se juega una carta Reverse y se invierte el sentido de los turnos
    }

    @Test
    @DisplayName("6. La carta Draw 2 obliga al siguiente jugador a robar 2 cartas y perder el turno")
    void drawTwoForcesDrawAndSkipsTurn() {
        // Simula Draw 2 y verifica que el siguiente jugador roba 2 y pierde su turno
    }

    @Test
    @DisplayName("7. La carta Wild permite elegir un color")
    void wildCardAllowsColorChange() {
        // Simula que se juega una Wild y se elige color
    }

    @Test
    @DisplayName("8. El juego termina cuando un jugador se queda sin cartas")
    void gameEndsWhenPlayerHasNoCards() {
        // Simula una situación donde un jugador juega su última carta
        // Verifica que el juego se marque como terminado
    }

    @Test
    @DisplayName("9. No se permite jugar después de que el juego ha terminado")
    void noPlayAllowedAfterGameEnds() {
        // Intenta jugar una carta después de terminado el juego, debe lanzar excepción o ser ignorado
    }

    @Test
    @DisplayName("10. El turno se pasa correctamente al siguiente jugador (normal y con dirección invertida)")
    void turnPassesCorrectlyWithAndWithoutReverse() {
        // Verifica que el orden de turnos respeta la dirección del juego (normal o invertida)
    }
}
