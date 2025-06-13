package org.udesa.tp4.controller;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultMatcher;
import org.udesa.tp4.model.Card;
import org.udesa.tp4.model.JsonCard;
import org.udesa.tp4.model.Match;
import org.udesa.tp4.model.NumberCard;
import org.udesa.tp4.service.Dealer;
import org.udesa.tp4.service.UnoService;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

@SpringBootTest
@AutoConfigureMockMvc
public class UnoControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private UnoService unoService;  // servicio real

    @MockBean
    private Dealer dealer;           // mockeamos Dealer para controlar el mazo

    private final ObjectMapper objectMapper = new ObjectMapper();

    private UUID matchId;

    private ArrayList<Card> blueDeck;

    @BeforeEach
    public void setup() {
        // Construir mazo azul para el dealer mockeado
        blueDeck = buildDeck(20);
        doReturn(blueDeck).when(dealer).fullDeck();


        // Crear nueva partida con jugadores reales
        matchId = unoService.newMatch(List.of("Alice", "Bob"));
    }


    // Funciones auxiliares corregidas según tu controlador:
    private ArrayList<Card> buildDeck(int count) {
        ArrayList<Card> deck = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            deck.add(new NumberCard("Blue", 6));
        }
        return deck;
    }
    private UUID createMatch() throws Exception {
//        UUID matchId = UUID.randomUUID();
        List<String> players = List.of("Alice", "Bob");

        when(unoService.newMatch(players)).thenReturn(matchId);

        String response = mockMvc.perform(post("/newmatch")
                        .contentType(MediaType.APPLICATION_FORM_URLENCODED)
                        .param("players", "Alice", "Bob"))
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();

        ObjectMapper mapper = new ObjectMapper();
        return mapper.readValue(response, UUID.class);
    }


    private String playCard(UUID matchId, String player, JsonCard card) throws Exception {

        return mockMvc.perform(post("/play/" + matchId + "/" + player)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(card)))
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();
    }


    private void playCardFailing(UUID matchId, String player, JsonCard card) throws Exception {

        mockMvc.perform(post("/play/" + matchId + "/" + player)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(card)))
                .andExpect(status().isBadRequest()) // 400
                .andExpect(content().string(containsString("Invalid play")));  // opcional para validar mensaje
    }





    private void drawCard(UUID matchId, String player, UUID expectedMatchId) throws Exception {


        mockMvc.perform(post("/draw/" + matchId + "/" + player))
                .andExpect(status().isOk())
                .andExpect(content().string("\"" + expectedMatchId.toString() + "\""));
    }





    private List<JsonCard> getPlayerHand(UUID matchId, List<JsonCard> mockedHand) throws Exception {

        String response = mockMvc.perform(get("/playerhand/" + matchId)
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();

        return objectMapper.readValue(response, new TypeReference<List<JsonCard>>() {});
    }

    private JsonCard getActiveCard(UUID matchId, Card activeCard) throws Exception {

        String json = mockMvc.perform(get("/activecard/" + matchId))
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();

        return objectMapper.readValue(json, JsonCard.class);
    }

    private void playCardExpectingFailure(UUID matchId, String player, Card card, String expectedErrorMsg) throws Exception {

        mockMvc.perform(post("/play/" + matchId + "/" + player)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(card.asJson())))
                .andExpect(status().is5xxServerError())
                .andExpect(content().string(containsString(expectedErrorMsg)));
    }
    private void drawCardExpectingFailure(UUID matchId, String player, String expectedErrorMsg) throws Exception {
//        when(unoService.drawCard(matchId, player))
//                .thenThrow(new IllegalArgumentException(expectedErrorMsg));

        mockMvc.perform(post("/draw/" + matchId + "/" + player))
                .andExpect(status().isBadRequest())
                .andExpect(content().string(containsString(expectedErrorMsg)));
    }
    private void getPlayerHandExpectingFailure(UUID matchId, String expectedErrorMsg) throws Exception {
//        when(unoService.playerHand(matchId))
//                .thenThrow(new RuntimeException(expectedErrorMsg));

        mockMvc.perform(get("/playerhand/" + matchId))
                .andExpect(status().is5xxServerError())
                .andExpect(content().string(containsString(expectedErrorMsg)));
    }
    private void playCardWithMalformedJson(UUID matchId, String player, String malformedJson) throws Exception {
        mockMvc.perform(post("/play/" + matchId + "/" + player)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(malformedJson))
                .andExpect(status().isBadRequest());
    }







    // --- TESTS ---

    @Test
    public void testCreateMatch() throws Exception {
        assertNotNull(matchId);
    }

    @Test
    public void testPlayCardSuccessfully() throws Exception {

        String player = "Alice";

        JsonCard cardToPlay = new JsonCard("Blue", 6, "NumberCard", false);

        playCard(matchId, player, cardToPlay);
    }
    @Test
    public void testPlayCardInvalidColor() throws Exception {

        String player = "Alice";

        Card cardToPlay = new NumberCard("Grey", 9);

        playCardExpectingFailure(matchId, player, cardToPlay,"Not a card in hand");
    }

    @Test
    public void testPlayCardFails() throws Exception {


        String player = "Alice";

        // Esta carta se supone que es inválida según la lógica de la partida (por ej. color o número no coinciden)
        JsonCard invalidCard = new JsonCard("Green", 0, "NumberCard", false);

        // El método playCardFailing ya prepara el mock y verifica que da 5xx
        playCardFailing(matchId, player, invalidCard);
    }

    @Test
    public void testPlayCardFails_GameIsOver() throws Exception {

        String player = "Alice";
        Card card = new NumberCard("Blue", 6);

        playCardExpectingFailure(matchId, player, card, "Game over");
    }

    @Test
    public void testPlayCardFails_PlayerHasNoCard() throws Exception {

        String player = "Alice";
        Card card = new NumberCard("Green", 0);

        playCardExpectingFailure(matchId, player, card, "Not a card in hand");
    }

    @Test
    public void testPlayCardFails_NotPlayersTurn() throws Exception {

        String player = "Bob";
        Card card = new NumberCard("Blue", 6);

        playCardExpectingFailure(matchId, player, card, "It is not turn of player");
    }




    @Test
    public void testDrawCardSuccessfullyWithHelper() throws Exception {

        String player = "Alice";

        drawCard(matchId, player, matchId);
    }


    @Test
    public void testGetPlayerHandReturnsCards() throws Exception {


        List<JsonCard> expectedHand = List.of(
                new JsonCard("Blue", 6, "NumberCard", false),
                new JsonCard("Blue", 6, "NumberCard", false),
                new JsonCard("Blue", 6, "NumberCard", false),
                new JsonCard("Blue", 6, "NumberCard", false),
                new JsonCard("Blue", 6, "NumberCard", false),
                new JsonCard("Blue", 6, "NumberCard", false),
                new JsonCard("Blue", 6, "NumberCard", false)

        );

        List<JsonCard> actualHand = getPlayerHand(matchId, expectedHand);

        assertEquals(7, actualHand.size());
        assertEquals("Blue", actualHand.get(0).getColor());
        assertEquals(6, actualHand.get(0).getNumber());
        assertEquals("Blue", actualHand.get(1).getColor());
        assertEquals(6, actualHand.get(1).getNumber());
    }

    @Test
    public void testGetActiveCard() throws Exception {


        Card activeCard = new NumberCard("Blue",6);

        Card returnedCard = getActiveCard(matchId, activeCard).asCard();

        assertEquals("Blue", returnedCard.asJson().getColor());
        assertEquals(6, returnedCard.asJson().getNumber());
    }
    @Test
    public void testDrawCardWithInvalidPlayerFails() throws Exception {
        drawCardExpectingFailure(matchId, "Bob", "It is not turn of player");
    }

    @Test
    public void testPlayCardWithInvalidCardFails() throws Exception {

        Card invalidCard = new NumberCard("Green", 3);
        playCardExpectingFailure(matchId, "Alice", invalidCard, "Not a card in hand");
    }

    @Test
    public void testPlayerHandFailsForInvalidMatch() throws Exception {
        UUID randomID = UUID.randomUUID();
        getPlayerHandExpectingFailure(randomID, "No se encontró una partida con el ID");
    }

    @Test
    public void testMalformedJsonFails() throws Exception {

        playCardWithMalformedJson(UUID.randomUUID(), "Alice", "{\"color\":\"Red\",\"number\":\"abc\"}");
    }

}


// new match no null
// chequear players new match
// chequear play: UUID matchId, String player, JsonCard card
// active card: chequear match
// draw card chequar cartas jugador: (UUID matchId, String player
//player hand: chequear match id