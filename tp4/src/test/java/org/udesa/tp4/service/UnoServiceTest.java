package org.udesa.tp4.service;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.udesa.tp4.model.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@SpringBootTest
public class UnoServiceTest {

    @Autowired
    UnoService unoService;

    @MockBean
    Dealer dealer;

    UUID matchId;
    Match spyMatch;
    ArrayList<Card> fullDeck;
    ArrayList<Card> blueDeck;
    @BeforeEach
    public void setup() {
        // Crear mazo azul para usar en el dealer
        blueDeck = buildDeck();

        // Mockeamos el dealer para que devuelva ese mazo cuando se llame fullDeck()
        when(dealer.fullDeck()).thenReturn(blueDeck);

        // Crear nueva partida con los jugadores y asignar el UUID al campo de instancia
        matchId = unoService.newMatch(List.of("Alice", "Bob"));

        // Obtenemos la partida que creó unoService y la "espiamos" para poder verificar métodos
        Match originalMatch = unoService.getMatch(matchId);
        spyMatch = spy(originalMatch);

        // Reemplazamos la partida en el servicio por el spy para poder verificar interacciones
        unoService.addMatch(spyMatch, matchId);
    }


    @Test
    public void newMatchTest() {
        assertNotNull(matchId);
    }

    @Test
    public void getMatch_shouldThrowIfNotFound() {
        UUID unknownId = UUID.randomUUID();
        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.getMatch(unknownId));
        assertTrue(ex.getMessage().contains("No matchID"));
    }

    @Test
    public void play_validCard_shouldCallMatchPlay() {
        JsonCard jsonCard = new JsonCard("Red", 7, "NumberCard", false);
        Card expectedCard = jsonCard.asCard();

        unoService.play(matchId, "Alice", jsonCard);

        verify(spyMatch, times(1)).play("Alice", expectedCard);
    }

    @Test
    public void play_shouldThrowIfMatchNotFound() {
        UUID unknownId = UUID.randomUUID();
        JsonCard card = new JsonCard("Green", 3, "NumberCard", false);

        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.play(unknownId, "Alice", card));
        assertTrue(ex.getMessage().contains("No matchID"));
    }
    @Test
    public void play_shouldThrowIfGameOver() {
        when(spyMatch.isOver()).thenReturn(true);

        JsonCard card = new JsonCard("Red", 7, "NumberCard", false);

        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.play(matchId, "Alice", card));
        assertTrue(ex.getMessage().contains("game over"));
    }

    @Test
    public void play_shouldTranslateKnownMatchExceptions() {
        JsonCard card = new JsonCard("Red", 7, "NumberCard", false);

        doThrow(new RuntimeException(Match.NotACardInHand + "Alice"))
                .when(spyMatch).play(anyString(), any(Card.class));
        RuntimeException ex1 = assertThrows(RuntimeException.class, () -> unoService.play(matchId, "Alice", card));
        assertTrue(ex1.getMessage().contains("Not a card in hand"));

        doThrow(new RuntimeException(Match.CardDoNotMatch))
                .when(spyMatch).play(anyString(), any(Card.class));
        RuntimeException ex2 = assertThrows(RuntimeException.class, () -> unoService.play(matchId, "Alice", card));
        assertTrue(ex2.getMessage().contains("Card does not match"));

        doThrow(new RuntimeException(Player.NotPlayersTurn + "Alice"))
                .when(spyMatch).play(anyString(), any(Card.class));
        RuntimeException ex3 = assertThrows(RuntimeException.class, () -> unoService.play(matchId, "Alice", card));
        assertTrue(ex3.getMessage().contains("It is not turn of player"));

        doThrow(new RuntimeException("Error raro"))
                .when(spyMatch).play(anyString(), any(Card.class));
        RuntimeException ex4 = assertThrows(RuntimeException.class, () -> unoService.play(matchId, "Alice", card));
        assertTrue(ex4.getMessage().contains("Error al jugar la carta"));
    }


    @Test
    public void activeCard_shouldReturnActiveCard() {
        Card activeCard = new NumberCard("Red", 7);
        when(spyMatch.activeCard()).thenReturn(activeCard);

        Card actual = unoService.activeCard(matchId);

        assertEquals(activeCard.asJson().getColor(), actual.asJson().getColor());
        assertEquals(activeCard.asJson().getNumber(), actual.asJson().getNumber());
    }

    @Test
    public void activeCard_shouldThrowIfMatchNotFound() {
        UUID unknownId = UUID.randomUUID();
        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.activeCard(unknownId));
        assertTrue(ex.getMessage().contains("No matchID"));
    }


    @Test
    public void drawCard_shouldCallMatchDrawCard() {
        unoService.drawCard(matchId, "Alice");
        verify(spyMatch, times(1)).drawCard("Alice");
    }

    @Test
    public void drawCard_shouldThrowIfMatchNotFound() {
        UUID unknownId = UUID.randomUUID();
        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.drawCard(unknownId, "Alice"));
        assertTrue(ex.getMessage().contains("No matchID"));
    }

    @Test
    public void drawCard_shouldThrowIfGameOver() {
        when(spyMatch.isOver()).thenReturn(true);

        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.drawCard(matchId, "Alice"));
        assertTrue(ex.getMessage().contains("game over"));
    }

    @Test
    public void drawCard_shouldTranslateKnownMatchExceptions() {
        doThrow(new RuntimeException(Player.NotPlayersTurn + "Alice"))
                .when(spyMatch).drawCard("Alice");
        RuntimeException ex1 = assertThrows(RuntimeException.class, () -> unoService.drawCard(matchId, "Alice"));
        assertTrue(ex1.getMessage().contains("turn"));

        doThrow(new RuntimeException("Error extraño"))
                .when(spyMatch).drawCard("Alice");
        RuntimeException ex2 = assertThrows(RuntimeException.class, () -> unoService.drawCard(matchId, "Alice"));
        assertTrue(ex2.getMessage().contains("Error al robar"));
    }

    private ArrayList<Card> buildDeck() {
        ArrayList<Card> deck = new ArrayList<>();
        String[] colors = {"Red", "Green", "Blue", "Yellow"};


        for (String color : colors) {
            deck.add(new NumberCard(color, 3));
            deck.add(new NumberCard(color, 7));
            deck.add(new NumberCard(color, 5));
            deck.add(new NumberCard(color, 1));
        }


        return deck;
    }

}

//    // chequear players new match
//    // chequear play: UUID matchId, String player, JsonCard card
//    // active card: chequear match
//    // draw card chequar cartas jugador: (UUID matchId, String player
//    //player hand: chequear match id
//
//
