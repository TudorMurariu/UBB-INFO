import java.util.*;
public class P2{
    /*
     * Să se determine distanța Euclideană între două locații 
     * identificate prin perechi de numere. De ex. distanța între 
     * (1,5) și (4,1) este 5.0.
     */
    private static class Point{
        public double x;
        public double y;
        public Point(double x, double y) {
            this.x = x;
            this.y = y;
        }
    }

    public static void main(String[] args) { 
        test();
    }

    private static void test() {
        Point A = new Point(1, 5);
        Point B = new Point(4, 1);
        assert(euclidianDist(A, B) == 5.0);
        A = new Point(5, 4);
        B = new Point(4, 5);
        assert(euclidianDist(A, B) == Math.sqrt(2));
        A = new Point(5, 3);
        B = new Point(3, 5);
        assert(euclidianDist(A, B) == 2);
    }

    // Time complexity: O(1)                            (Math.sqrt)
    // Space complexity: O(1)
    private static double euclidianDist(Point a, Point b) {
        return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
    }
}
