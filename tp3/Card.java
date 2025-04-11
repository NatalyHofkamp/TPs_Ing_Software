
public class Card {
    protected Color color;

    public Card(Color color) {
        this.color = color;
    }
    public abstract boolean PlayOn(Card topCard);
    public Color getColor() {
        return color;
    }
}


public class NumberCard extends Card {
    public int number;

    public NumberCard(Color color, int number) {
        super(color);
        this.number = number;
    }

    public boolean PlayOn(Card topCard) {
        return this.color.matches(topCard.getColor()) ||
                (topCard instanceof NumberCard && this.number == ((NumberCard) topCard).number) ||
                (topCard instanceof WildCard);
    }

    public void applyEffect(Game game) {
        // No efecto especial
    }
}


public class drawTwoCard extends Card {
    public drawTwoCard(Color color) {
        super(color);
    }

    public boolean PlayOn(Card topCard) {
        return this.color.matches(topCard.getColor()) ||
                (topCard instanceof WildCard) || (topCard instanceof drawTwoCard);
    }

    public void applyEffect(Game game) {
        // Logic to make next player draw two cards
    }
}

public class reverseCard extends Card {
    public reverseCard(Color color) {
        super(color);
    }

    public boolean PlayOn(Card topCard) {
        return this.color.matches(topCard.getColor()) ||
                (topCard instanceof WildCard) || (topCard instanceof reverseCard);
    }

    public void applyEffect(Game game) {
        // Logic to reverse the game's turn order
    }
}
}

public class skipCard extends Card {
    public skipCard(Color color) {
        super(color);
    }

    public boolean PlayOn(Card topCard) {
        return this.color.matches(topCard.getColor()) ||
                (topCard instanceof skipCard)||
                (topCard instanceof WildCard);
    }


    public void applyEffect(Game game) {
        // Skip the next player's turn logic here
    }
}

public class WildCard extends Card {
    public WildCard() {
        super(new WildColor()); // color especial
    }

    public boolean PlayOn(Card topCard) {
        return true;
    }

    public void applyEffect(Game game) {
        // Elige color (en tests podrías simularlo)
    }
}
