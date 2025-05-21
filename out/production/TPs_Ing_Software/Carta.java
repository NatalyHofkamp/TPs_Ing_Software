
public abstract class Carta {
    protected String color;

    public Carta(String color) {
        this.color = color;
    }
    public abstract boolean PlayOn(Carta topCard);
    public String getColor() {
        return color;
    }
    public abstract GameUNO applyEffect(GameUNO game);
}


class NumberCard extends Carta {
    public int number;

    public NumberCard(String color, int number) {
        super(color);
        this.number = number;
    }

    public boolean PlayOn(Carta topCard) {
        return this.color.matches(topCard.getColor()) ||
                (topCard instanceof NumberCard && this.number == ((NumberCard) topCard).number) ||
                (topCard instanceof WildCard);
    }

    public GameUNO applyEffect(GameUNO game) {
        // No efecto especial
        return game;
    }
}


class drawTwoCard extends Carta {
    public drawTwoCard(String color) {
        super(color);
    }

    public boolean PlayOn(Carta topCard) {
        return this.color.matches(topCard.getColor()) ||
                (topCard instanceof WildCard) || (topCard instanceof drawTwoCard);
    }

    public GameUNO applyEffect(GameUNO game) {
        game.drawCards(2); // El siguiente jugador roba 2 cartas
        game.skipNextPlayer();// Salta el turno del siguiente jugador
        return game;
    }

}

class reverseCard extends Carta {
    public reverseCard(String color) {
        super(color);
    }

    public boolean PlayOn(Carta topCard) {
        return this.color.matches(topCard.getColor()) ||
                (topCard instanceof WildCard) || (topCard instanceof reverseCard);
    }

    public GameUNO applyEffect(GameUNO game) {
        // Logic to reverse the game's turn order
        return game.reverseTurnOrder();
    }
}


class skipCard extends Carta {
    public skipCard(String color) {
        super(color);
    }

    public boolean PlayOn(Carta topCard) {
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

class WildCard extends Carta {
    private String color;
    public WildCard() {
        super(null); // aún no tiene color
    }

    public boolean PlayOn(Carta topCard) {
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
