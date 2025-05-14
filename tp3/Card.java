
public abstract class Card {
    protected String color;

    public Card(String color) {
        this.color = color;
    }
    public abstract boolean PlayOn(Card topCard);
    public String getColor() {
        return color;
    }
    public abstract GameUNO applyEffect(GameUNO game);
}


class NumberCard extends Card {
    public int number;

    public NumberCard(String color, int number) {
        super(color);
        this.number = number;
    }

    public boolean PlayOn(Card topCard) {
        return this.color.matches(topCard.getColor()) ||
                (topCard instanceof NumberCard && this.number == ((NumberCard) topCard).number) ||
                (topCard instanceof WildCard);
    }

    public GameUNO applyEffect(GameUNO game) {
        // No efecto especial
        return game;
    }
}


class drawTwoCard extends Card {
    public drawTwoCard(String color) {
        super(color);
    }

    public boolean PlayOn(Card topCard) {
        return this.color.matches(topCard.getColor()) ||
                (topCard instanceof WildCard) || (topCard instanceof drawTwoCard);
    }

    public GameUNO applyEffect(GameUNO game) {
        game.drawCards(2); // El siguiente jugador roba 2 cartas
        game.skipNextPlayer();// Salta el turno del siguiente jugador
        return game;
    }

}

class reverseCard extends Card {
    public reverseCard(String color) {
        super(color);
    }

    public boolean PlayOn(Card topCard) {
        return this.color.matches(topCard.getColor()) ||
                (topCard instanceof WildCard) || (topCard instanceof reverseCard);
    }

    public GameUNO applyEffect(GameUNO game) {
        // Logic to reverse the game's turn order
        return game.reverseTurnOrder();
    }
}


class skipCard extends Card {
    public skipCard(String color) {
        super(color);
    }

    public boolean PlayOn(Card topCard) {
        return this.color.matches(topCard.getColor());
//        return this.color.matches(topCard.getColor()) ||
//                (topCard instanceof skipCard)||
//                (topCard instanceof WildCard);
    }


    public GameUNO applyEffect(GameUNO game) {
        // Skip the next player's turn logic here
        return game.skipNextPlayer();
    }
}

class WildCard extends Card {
    private String color;
    public WildCard() {
        super(null); // aún no tiene color
    }

    public boolean PlayOn(Card topCard) {
        return true;
    }
    public WildCard asColor(String color) {
        this.color = color;
        return this;
    }

    public GameUNO applyEffect(GameUNO game) {
        // Elige color (en tests podrías simularlo)
        return game;
    }
}
