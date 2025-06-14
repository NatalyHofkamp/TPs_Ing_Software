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

    private ArrayList<Card> myDeck;

    @BeforeEach
    public void setup() {
        // Construir mazo azul para el dealer mockeado
        myDeck = buildDeck();
        doReturn(myDeck).when(dealer).fullDeck();
        matchId = unoService.newMatch(List.of("Alice", "Bob"));
    }


    // Funciones auxiliares corregidas según tu controlador:
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
                .andExpect(status().is5xxServerError())
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

        mockMvc.perform(post("/draw/" + matchId + "/" + player))
                .andExpect(status().is5xxServerError())
                .andExpect(content().string(containsString(expectedErrorMsg)));
    }
    private void getPlayerHandExpectingFailure(UUID matchId, String expectedErrorMsg) throws Exception {

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

        JsonCard cardToPlay = new JsonCard("Green", 3, "NumberCard", false);

        playCard(matchId, player, cardToPlay);
    }
    @Test
    public void testPlayCardInvalidColor() throws Exception {

        String player = "Alice";

        Card cardToPlay = new NumberCard("Grey", 9);

        playCardExpectingFailure(matchId, player, cardToPlay,"Not a card in hand");
    }

    @Test
    public void testPlayCardFails_PlayerHasNoCard() throws Exception {

        String player = "Alice";
        Card card = new NumberCard("Green", 0);
//        playCard(matchId, player, cardToPlay);
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
        assertEquals("Red", actualHand.get(0).getColor());
        assertEquals(7, actualHand.get(0).getNumber());
        assertEquals("Red", actualHand.get(1).getColor());
        assertEquals(5, actualHand.get(1).getNumber());
    }

    @Test
    public void testGetActiveCard() throws Exception {


        Card activeCard = new NumberCard("Blue",3);

        Card returnedCard = getActiveCard(matchId, activeCard).asCard();

        assertEquals("Red", returnedCard.asJson().getColor());
        assertEquals(3, returnedCard.asJson().getNumber());
    }
    @Test
    public void testDrawCardWithInvalidPlayerFails() throws Exception {
        drawCardExpectingFailure(matchId, "Bob", "It is not turn of player");
    }

    @Test
    public void testPlayCardWithInvalidCardFails() throws Exception {

        Card invalidCard = new NumberCard("Green", 7);
        playCardExpectingFailure(matchId, "Alice", invalidCard, "");
    }

    @Test
    public void testPlayerHandFailsForInvalidMatch() throws Exception {
        UUID randomID = UUID.randomUUID();
        getPlayerHandExpectingFailure(randomID, "No matchID");
    }

    @Test
    public void testMalformedJsonFails() throws Exception {

        playCardWithMalformedJson(UUID.randomUUID(), "Alice", "{\"color\":\"Red\",\"number\":\"abc\"}");
    }

    @Test
    public void testPlayCardFails_GameIsOver() throws Exception {
        String alice = "Alice";
        String bob = "Bob";

        // Mazo con pila inicial + 7 cartas para Alice + 7 cartas para Bob
        ArrayList<Card> newDeck = new ArrayList<>(List.of(
                new NumberCard("Red", 3),     // pila central (descartes)

                // Cartas de Alice
                new NumberCard("Red", 7),
                new NumberCard("Yellow", 2),
                new NumberCard("Green", 7),
                new NumberCard("Blue", 4),
                new NumberCard("Green", 9),
                new NumberCard("Yellow", 1),
                new NumberCard("Red", 2),

                // Cartas de Bob
                new NumberCard("Red", 2),
                new NumberCard("Yellow", 7),
                new NumberCard("Green", 4),
                new NumberCard("Blue", 9),
                new NumberCard("Green", 1),
                new NumberCard("Red", 1),
                new NumberCard("Blue", 2)
        ));

        // Mockear el dealer para devolver ese mazo
        doReturn(newDeck).when(dealer).fullDeck();

        // Crear el match *aquí* para que tome el mazo correcto
        matchId = unoService.newMatch(List.of(alice, bob));

        // Jugar todas las cartas de Alice y Bob alternadamente
        for (int i = 0; i < 7; i++) {
            Card aliceCard = newDeck.get(i + 1);
            JsonCard aliceJsonCard = new JsonCard(
                    aliceCard.asJson().getColor(),
                    aliceCard.asJson().getNumber(),
                    aliceCard.asJson().getType(),
                    (i == 5 || i == 6)  // shout true solo en anteúltimas jugadas
            );
            System.out.println("Alice juega: " + aliceJsonCard.getColor() + " " + aliceJsonCard.getNumber() + " shout:" + aliceJsonCard.isShout());
            playCard(matchId, "Alice", aliceJsonCard);

            Card bobCard = newDeck.get(i + 8);
            JsonCard bobJsonCard = new JsonCard(
                    bobCard.asJson().getColor(),
                    bobCard.asJson().getNumber(),
                    bobCard.asJson().getType(),
                    (i == 5 || i == 6)
            );
            System.out.println("Bob juega: " + bobJsonCard.getColor() + " " + bobJsonCard.getNumber() + " shout:" + bobJsonCard.isShout());

            if (i < 6) {
                // Para las primeras 6 cartas de Bob, juega normalmente
                playCard(matchId, "Bob", bobJsonCard);
            } else {
                // Para la última carta de Bob, esperamos que falle porque el juego terminó
                playCardExpectingFailure(matchId, "Bob", bobJsonCard.asCard(), "game over");
            }
        }

    }

}

