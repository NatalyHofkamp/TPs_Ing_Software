package org.udesa.tp4.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.udesa.tp4.model.Card;
import org.udesa.tp4.model.JsonCard;
import org.udesa.tp4.model.Match;
import org.udesa.tp4.model.Player;

import java.util.*;

@Service
public class UnoService {
    private Map<UUID, Match> sessions = new HashMap<>();

    @Autowired
    Dealer dealer;

    public UUID newMatch(List<String> players) {
        UUID newKey = UUID.randomUUID();
        sessions.put(newKey, Match.fullMatch(dealer.fullDeck(), players));
        return newKey;
    }
    public void addMatch(Match match, UUID sessionId) {sessions.put(sessionId, match);}

    public Match getMatch(UUID matchId) {
        Match match = sessions.get(matchId);
        if (match == null) {
            throw new RuntimeException("No matchID: " + matchId);
        }
        return match;
    }

    public UUID play(UUID matchId, String player, JsonCard card) {
        Match match = sessions.get(matchId);
        if (match == null) {
            throw new RuntimeException("No matchID: " + matchId);
        }


        if (match.isOver()) {
            throw new RuntimeException("game over");
        }

        try {
            match.play(player, card.asCard());
            sessions.put(matchId, match);
            return matchId;
        } catch (RuntimeException e) {
            if (e.getMessage().equals(Match.NotACardInHand + player)) {
                throw new RuntimeException(Match.NotACardInHand  + e.getMessage());
            } else if (e.getMessage().equals(Match.CardDoNotMatch)) {
                throw new RuntimeException(Match.CardDoNotMatch + e.getMessage());
            } else if (e.getMessage().startsWith(Player.NotPlayersTurn)) {
                throw new RuntimeException(Player.NotPlayersTurn + player);
            }
            else {
                throw new RuntimeException("Error playcard: " + e.getMessage()); // Generic error
            }
        }
    }

    public Card activeCard(UUID matchId) {
        Match match = sessions.get(matchId);
        if (match == null) {
            throw new RuntimeException("No matchID " + matchId);
        }

        try {
            return match.activeCard();
        } catch (Exception e) {
            throw new RuntimeException("Error activecard: " + e.getMessage());
        }
    }

    public UUID drawCard(UUID matchId, String player) {
        Match match = sessions.get(matchId);
        if (match == null) {
            throw new RuntimeException("No matchID " + matchId);
        }

        if (match.isOver()) {
            throw new RuntimeException("game over");
        }

        try {
            match.drawCard(player);
            sessions.put(matchId, match);
            return matchId;
        } catch (RuntimeException e) {
            if (e.getMessage().startsWith(Player.NotPlayersTurn)) {
                throw new RuntimeException(Player.NotPlayersTurn + player);
            }
            else {
                throw new RuntimeException("Error drawcard:" + e.getMessage());
            }
        }
    }
    public List<JsonCard> playerHand(UUID matchId) {
        Match match = sessions.get(matchId);
        if (match == null) {
            throw new RuntimeException("No matchID: " + matchId);
        }

        try {
            List<Card> hand = match.playerHand();
            List<JsonCard> cards = new ArrayList<>();
            for (Card card : hand) {
                cards.add(card.asJson());
            }

            return cards;
        } catch (Exception e) {
            throw new RuntimeException("Error playerhand: " + e.getMessage());
        }
    }
}
