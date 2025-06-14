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
    ArrayList<Card> myDeck;
    @BeforeEach
    public void setup() {

        myDeck = buildDeck();

        when(dealer.fullDeck()).thenReturn(myDeck);

        matchId = unoService.newMatch(List.of("botcito", "Bob"));

        Match originalMatch = unoService.getMatch(matchId);
        spyMatch = spy(originalMatch);

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

        unoService.play(matchId, "botcito", jsonCard);

        verify(spyMatch, times(1)).play("botcito", expectedCard);
    }

    @Test
    public void play_shouldThrowIfMatchNotFound() {
        UUID unknownId = UUID.randomUUID();
        JsonCard card = new JsonCard("Green", 3, "NumberCard", false);

        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.play(unknownId, "botcito", card));
        assertTrue(ex.getMessage().contains("No matchID"));
    }
    @Test
    public void play_shouldThrowIfGameOver() {
        when(spyMatch.isOver()).thenReturn(true);

        JsonCard card = new JsonCard("Red", 7, "NumberCard", false);

        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.play(matchId, "botcito", card));
        assertTrue(ex.getMessage().contains("game over"));
    }

    @Test
    public void play_shouldTranslateKnownMatchExceptions() {
        JsonCard card = new JsonCard("Red", 7, "NumberCard", false);

        doThrow(new RuntimeException(Match.NotACardInHand + "botcito"))
                .when(spyMatch).play(anyString(), any(Card.class));
        RuntimeException ex1 = assertThrows(RuntimeException.class, () -> unoService.play(matchId, "botcito", card));
        assertTrue(ex1.getMessage().contains("Not a card in hand"));

        doThrow(new RuntimeException(Match.CardDoNotMatch))
                .when(spyMatch).play(anyString(), any(Card.class));
        RuntimeException ex2 = assertThrows(RuntimeException.class, () -> unoService.play(matchId, "botcito", card));
        assertTrue(ex2.getMessage().contains("Card does not match"));

        doThrow(new RuntimeException(Player.NotPlayersTurn + "botcito"))
                .when(spyMatch).play(anyString(), any(Card.class));
        RuntimeException ex3 = assertThrows(RuntimeException.class, () -> unoService.play(matchId, "botcito", card));
        assertTrue(ex3.getMessage().contains("It is not turn of player"));

        doThrow(new RuntimeException("Error raro"))
                .when(spyMatch).play(anyString(), any(Card.class));
        RuntimeException ex4 = assertThrows(RuntimeException.class, () -> unoService.play(matchId, "botcito", card));
        assertTrue(ex4.getMessage().contains("Error playcard"));
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
        unoService.drawCard(matchId, "botcito");
        verify(spyMatch, times(1)).drawCard("botcito");
    }

    @Test
    public void drawCard_shouldThrowIfMatchNotFound() {
        UUID unknownId = UUID.randomUUID();
        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.drawCard(unknownId, "botcito"));
        assertTrue(ex.getMessage().contains("No matchID"));
    }

    @Test
    public void drawCard_shouldThrowIfGameOver() {
        when(spyMatch.isOver()).thenReturn(true);

        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.drawCard(matchId, "botcito"));
        assertTrue(ex.getMessage().contains("game over"));
    }

    @Test
    public void drawCard_shouldTranslateKnownMatchExceptions() {
        doThrow(new RuntimeException(Player.NotPlayersTurn + "botcito"))
                .when(spyMatch).drawCard("botcito");
        RuntimeException ex1 = assertThrows(RuntimeException.class, () -> unoService.drawCard(matchId, "botcito"));
        assertTrue(ex1.getMessage().contains("turn"));

        doThrow(new RuntimeException("Error extraño"))
                .when(spyMatch).drawCard("botcito");
        RuntimeException ex2 = assertThrows(RuntimeException.class, () -> unoService.drawCard(matchId, "botcito"));
        assertTrue(ex2.getMessage().contains("Error drawcard"));
    }
    @Test
    public void playerHand_shouldReturnJsonCards() {
        List<Card> hand = List.of(
                new NumberCard("Red", 5),
                new NumberCard("Blue", 2)
        );
        when(spyMatch.playerHand()).thenReturn(hand);

        List<JsonCard> result = unoService.playerHand(matchId);

        assertEquals(2, result.size());
        assertEquals("Red", result.get(0).getColor());
        assertEquals(5, result.get(0).getNumber());
        assertEquals("Blue", result.get(1).getColor());
        assertEquals(2, result.get(1).getNumber());
    }
    @Test
    public void playerHand_shouldThrowIfMatchNotFound() {
        UUID unknownId = UUID.randomUUID();

        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.playerHand(unknownId));
        assertTrue(ex.getMessage().contains("No matchID"));
    }

    @Test
    public void playerHand_shouldWrapException() {
        when(spyMatch.playerHand()).thenThrow(new RuntimeException("algo explotó"));

        RuntimeException ex = assertThrows(RuntimeException.class, () -> unoService.playerHand(matchId));
        assertTrue(ex.getMessage().contains("Error playerhand"));
        assertTrue(ex.getMessage().contains("algo explotó"));
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
