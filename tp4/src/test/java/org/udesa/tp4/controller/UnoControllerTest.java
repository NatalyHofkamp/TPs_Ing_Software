package org.udesa.tp4.controller;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultMatcher;
import org.udesa.tp4.model.JsonCard;
import org.udesa.tp4.service.UnoService;

import java.util.List;
import java.util.UUID;

import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
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

    @MockBean
    private UnoService unoService;

    private final ObjectMapper objectMapper = new ObjectMapper();

    // Funciones auxiliares corregidas según tu controlador:

    private UUID createMatch() throws Exception {
        UUID matchId = UUID.randomUUID();
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
        UUID dummyUuid = UUID.randomUUID();
        when(unoService.play(matchId, player, card)).thenReturn(dummyUuid);

        return mockMvc.perform(post("/play/" + matchId + "/" + player)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(card)))
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();
    }


    private void playCardFailing(UUID matchId, String player, JsonCard card) throws Exception {
        when(unoService.play(matchId, player, card)).thenThrow(new RuntimeException("Invalid play"));

        mockMvc.perform(post("/play/" + matchId + "/" + player)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(card)))
                .andExpect(status().is5xxServerError());
    }




    private void drawCard(UUID matchId, String player, UUID expectedMatchId) throws Exception {
        when(unoService.drawCard(matchId, player)).thenReturn(expectedMatchId);

        mockMvc.perform(post("/draw/" + matchId + "/" + player))
                .andExpect(status().isOk())
                .andExpect(content().string("\"" + expectedMatchId.toString() + "\""));
    }





    private List<JsonCard> getPlayerHand(UUID matchId, List<JsonCard> mockedHand) throws Exception {
        when(unoService.playerHand(matchId)).thenReturn(mockedHand);

        String response = mockMvc.perform(get("/playerhand/" + matchId)
                        .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();

        return objectMapper.readValue(response, new TypeReference<List<JsonCard>>() {});
    }

    private JsonCard getActiveCard(UUID matchId, JsonCard activeCard) throws Exception {
        when(unoService.activeCard(matchId)).thenReturn(activeCard);

        String json = mockMvc.perform(get("/activecard/" + matchId))
                .andExpect(status().isOk())
                .andReturn()
                .getResponse()
                .getContentAsString();

        return objectMapper.readValue(json, JsonCard.class);
    }


    // --- TESTS ---

    @Test
    public void testCreateMatch() throws Exception {
        UUID matchId = createMatch();
        assertNotNull(matchId);
    }

    @Test
    public void testPlayCardSuccessfully() throws Exception {
        UUID matchId = UUID.randomUUID();
        String player = "Alice";

        JsonCard cardToPlay = new JsonCard("Blue", 5, "NumberCard", false);

        playCard(matchId, player, cardToPlay);
    }

    @Test
    public void testPlayCardFails() throws Exception {
        UUID matchId = UUID.randomUUID();
        String player = "Bob";

        // Esta carta se supone que es inválida según la lógica de la partida (por ej. color o número no coinciden)
        JsonCard invalidCard = new JsonCard("Red", 20, "NumberCard", false);

        // El método playCardFailing ya prepara el mock y verifica que da 5xx
        playCardFailing(matchId, player, invalidCard);
    }


    @Test
    public void testDrawCardSuccessfullyWithHelper() throws Exception {
        UUID matchId = UUID.randomUUID();
        String player = "Bob";
        UUID expectedUuid = UUID.randomUUID();

        drawCard(matchId, player, expectedUuid);
    }


    @Test
    public void testGetPlayerHandReturnsCards() throws Exception {
        UUID matchId = UUID.randomUUID();

        List<JsonCard> expectedHand = List.of(
                new JsonCard("Red", 5, "NumberCard", false),
                new JsonCard("Blue", 2, "NumberCard", false)
        );

        List<JsonCard> actualHand = getPlayerHand(matchId, expectedHand);

        assertEquals(2, actualHand.size());
        assertEquals("Red", actualHand.get(0).getColor());
        assertEquals(5, actualHand.get(0).getNumber());
        assertEquals("Blue", actualHand.get(1).getColor());
        assertEquals(2, actualHand.get(1).getNumber());
    }

    @Test
    public void testGetActiveCard() throws Exception {
        UUID matchId = UUID.randomUUID();

        JsonCard activeCard = new JsonCard("Yellow", 9, "NumberCard", false);

        JsonCard returnedCard = getActiveCard(matchId, activeCard);

        assertEquals("Yellow", returnedCard.getColor());
        assertEquals(9, returnedCard.getNumber());
    }
}


// new match no null
// chequear players new match
// chequear play: UUID matchId, String player, JsonCard card
// active card: chequear match
// draw card chequar cartas jugador: (UUID matchId, String player
//player hand: chequear match id