package org.udesa.tp4.service;

import org.springframework.stereotype.Component;
import org.udesa.tp4.model.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Component
public class Dealer {
    public List<Card> fullDeck() {
        ArrayList<Card> deck = new ArrayList<Card>();
        deck.addAll(cardsOn("Red"));
        deck.addAll(cardsOn("Blue"));
        deck.addAll(cardsOn("Green"));
        deck.addAll(cardsOn("Yellow"));
        Collections.shuffle(deck);
        return deck;
    }

    public List<Card> blueDeck() {
        ArrayList<Card> deck = new ArrayList<Card>();
//        deck.addAll(cardsOn("Red"));
        deck.addAll(cardsOnBlue("Blue"));
        deck.addAll(cardsOnBlue("Red"));
//        deck.addAll(cardsOn("Green"));
//        deck.addAll(cardsOn("Yellow"));
        Collections.shuffle(deck);
        return deck;
    }

    private List<Card> cardsOnBlue( String color){
        return List.of(new NumberCard(color, 6),new NumberCard(color, 6),
                new NumberCard(color, 6),new NumberCard(color, 6),
                new NumberCard(color, 6),new NumberCard(color, 6),
                new NumberCard(color, 6),new NumberCard(color, 6),
                new NumberCard(color, 6),new NumberCard(color, 6));
    }

    private List<Card> cardsOn( String color){
        return List.of(new NumberCard(color, 0),new NumberCard(color, 1),
                new NumberCard(color, 2),new NumberCard(color, 3),
                new NumberCard(color, 4),new NumberCard(color, 5),
                new NumberCard(color, 6),new NumberCard(color, 7),
                new NumberCard(color, 8),new NumberCard(color, 9),
                new Draw2Card(color), new SkipCard(color), new ReverseCard(color), new WildCard());
    }
}

//new WildCard(), new SkipCard(color), new Draw2Card(color),
//                new NumberCard(color, 0), new NumberCard(color, 1),
//                new NumberCard(color,2 ), new NumberCard(color, 3),
//                new NumberCard(color, 4), new NumberCard(color, 5),
//                new NumberCard(color, 6),new NumberCard(color, 7),
//                new NumberCard(color, 8), new NumberCard(color, 9)