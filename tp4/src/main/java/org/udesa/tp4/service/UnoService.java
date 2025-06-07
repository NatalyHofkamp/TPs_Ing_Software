package org.udesa.tp4.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.udesa.tp4.model.Card;
import org.udesa.tp4.model.JsonCard;
import org.udesa.tp4.model.Match;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
public class UnoService {
    private Map<UUID, Match> sessions = new HashMap<UUID, Match>();
    @Autowired
    Dealer dealer;
    public UUID newMatch(List<String> players) {
        UUID newKey = UUID.randomUUID();
        sessions.put(newKey, Match.fullMatch(dealer.fullDeck(), players));
        return newKey;
    }
    public UUID play(UUID matchId, String player, JsonCard card){
        Match match = sessions.get(matchId);
        if (match == null) {return null;}
        else{
            match.play(player, card.asCard());
            sessions.put( matchId, match);
            return matchId;
        }

    }
    public JsonCard activeCard(UUID matchId){
        Match match = sessions.get(matchId);
        if (match == null) {return null;}
        else{
            return match.activeCard().asJson();
        }

    }
}
