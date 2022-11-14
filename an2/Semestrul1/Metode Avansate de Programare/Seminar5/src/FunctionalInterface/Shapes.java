package FunctionalInterface;

import java.util.ArrayList;
import java.util.List;

public class Shapes {
    public static void main(String[] args) {
        Area<Circle> circleArea =
                (Circle c) ->
                (float) (Math.PI * c.getRadius() * c.getRadius());

        Circle circle = new Circle(1);
        float arieCircle = circleArea.computeArea(circle);
        System.out.println(arieCircle);

        List<Circle> circleList = new ArrayList<>();
        circleList.add(circle);
        circleList.add(new Circle(2.3f));

        System.out.println("Print lista:\n");
        printArie(circleList, circleArea);

        Area<Square> squareArea =
                (s) ->
                (s.getEdge() * s.getEdge());

        String as;
    }

    /**
     * ASDFASDFASD ASDF ASDF ASD F
     * @param l asdfa sdfa sdf asd
     * @param f  asdf asdf asdf sad fasd f
     * @param <E> asdf asdf asd fasdf sadf
     */
    public static <E> void printArie(List<E> l, Area<E> f) {
        l.forEach(x -> System.out.println(f.computeArea(x)));
    }
}

class Circle {
    private float radius;

    public Circle(float radius) {
        this.radius = radius;
    }

    public float getRadius() {
        return radius;
    }

    public void setRadius(float radius) {
        this.radius = radius;
    }
}

class Square {
    private float edge;

    public Square(float edge) {
        this.edge = edge;
    }

    public float getEdge() {
        return edge;
    }

    public void setEdge(float edge) {
        this.edge = edge;
    }
}