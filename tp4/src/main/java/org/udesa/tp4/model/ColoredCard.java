package org.udesa.tp4.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public abstract class ColoredCard extends Card {
    protected String color = "";
    private List<String> colors = new ArrayList<>(List.of("Red", "Green", "Blue", "Yellow"));

    public ColoredCard( String aColor ) {
        color = aColor;
//        try {
//            int index = colors.indexOf(aColor);
//            color = colors.get(index); // Esto lanza IndexOutOfBoundsException si index == -1
//        } catch (IndexOutOfBoundsException e) {
//            throw new RuntimeException("Color not valid: " + aColor);
//        }

    }
    public boolean acceptsOnTop( Card aCard ) { return  aCard.yourColorIs( color() );   }
    public boolean yourColorIs( String aColor ) { return color.equals( aColor );  }
    public String color() { return color;  }

    public boolean equals( Object o ) { return super.equals( o ) && color.equals( ColoredCard.class.cast( o ).color );  }
    public int hashCode() {             return Objects.hash( color );}

    public JsonCard asJson() { return new JsonCard( color, null, getClass().getSimpleName(), unoShouted() ); }
}
