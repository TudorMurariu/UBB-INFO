import java.util.*;

public class Main {
    public static void main(String[] args) {
        // e1();
        // e2();
        // e3();
        e4();
        // e5();
    }

    private static void e1() {
        System.out.println("Hello World!");
    }

    private static void e2() {
        Scanner cin = new Scanner(System.in);

        String s = cin.next();

        int i = cin.nextInt();

        float f = cin.nextFloat();

        System.out.println(s + " " +  i + " " + f);
        cin.close();
    }

    private static void e3() {
        Scanner cin = new Scanner(System.in);
        int n = cin.nextInt();
        //List<Integer> list = new ArrayList<>();


        int max = cin.nextInt();
        int min = max;
        for(int i=1;i < n;++i) {
            int x = cin.nextInt();
            if(x > max)
                max = x;
            if(x < min)
                min = x;
        }

        System.out.println("Max : " + max + "\nMin : " + min);
        cin.close();
    }

    private static  void e4() {
        Scanner cin = new Scanner(System.in);

        Cube c = new Cube();

        c.setLength(cin.nextFloat());

        System.out.println(c.area());

        cin.close();
    }

    private static void e5() {
        Employee e1 = new Employee("Andrei", 20, 3000.0f, "JuniorDev");
        Employee e2 = new Employee("Gabi", 21, 20000.0f, "Product Manager");

        System.out.println(e1);
        System.out.println(e1.getName() + " " + e1.getAge() + " " + e1.getSalary() + " " + e1.getPosition());
        System.out.println(e2);
        System.out.println(e2.getName() + " " + e2.getAge() + " " + e2.getSalary() + " " + e2.getPosition());
    }
}