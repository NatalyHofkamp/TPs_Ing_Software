package org.udesa.tp4.model;

import java.util.List;
import java.util.Objects;
import java.util.stream.IntStream;

public class NumberCard extends ColoredCard {
    private int number;

    private List<Integer> numbers = IntStream.rangeClosed(0, 9)
            .boxed()
            .toList();
    public static Card asCard( JsonCard aJson ) {
        return new NumberCard( aJson.getColor(), aJson.getNumber() ).shoutAs( aJson.isShout() );
    }

    public NumberCard( String aColor, int aNumber ) {
        super( aColor );
        try {
            int index = numbers.indexOf(aNumber);
            number = numbers.get(index); // Esto lanza IndexOutOfBoundsException si index == -1
        } catch (IndexOutOfBoundsException e) {
            throw new RuntimeException("Number not valid");
        }
    }

    public boolean acceptsOnTop( Card aCard ) { return super.acceptsOnTop( aCard ) || aCard.yourNumberIs( number );}
    public boolean yourNumberIs( int number ) { return number == this.number;   }

    public boolean equals( Object o ) { return super.equals( o ) && number == ((NumberCard) o).number; }
    public int hashCode() {             return Objects.hash( super.hashCode(), number );}

    public JsonCard asJson() { return new JsonCard( color, number, getClass().getSimpleName(), unoShouted() ); }

}
