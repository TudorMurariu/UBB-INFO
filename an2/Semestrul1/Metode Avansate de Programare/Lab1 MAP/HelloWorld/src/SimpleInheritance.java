import java.util.Scanner;


public class SimpleInheritance {
    public static void main(String[] args) {
        Scanner cin = new Scanner(System.in);

        Cube c = new Cube();

        c.setLength(cin.nextFloat());

        System.out.println(c.area());

        cin.close();
    }
}

class Square {
    protected float length;

    public float getLength() {
        return length;
    }

    public void setLength(float length) {
        this.length = length;
    }

    public float area() {
        return length * length;
    }
}

class Cube extends Square {

    @Override
    public float area() {
        return length * length * length;
    }
}