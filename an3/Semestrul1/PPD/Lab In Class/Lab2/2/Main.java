import java.util.ArrayList;
import java.util.List;

class Main {

    public static int THREAD_NUMBER = 4;

    public static List<Integer> a = new ArrayList<>(List.of(1, 2, 3, 6, 7));
    public static List<Integer> b = new ArrayList<>(List.of(2, 3, 4, 6, 7));
    public static List<Integer> c = new ArrayList<>(List.of(0, 0, 0, 0, 0));

    public static void main(String[] args) {

        System.out.print("A : ");
        for (Integer elem : a) 
            System.out.print(elem + " ");

        System.out.print("\nB : ");
        for (Integer elem : b) 
            System.out.print(elem + " ");

        System.out.println();
        Thread[] threads = new Thread[THREAD_NUMBER];
        for(int i=0;i<THREAD_NUMBER;++i) {
            threads[i] = new MyThread(i, THREAD_NUMBER);
            threads[i].start();
        }
        
        for(int i=0;i<THREAD_NUMBER;++i) {
            try {
                threads[i].join();
            } catch(InterruptedException e) {
                e.printStackTrace();
            }
        }

        System.out.print("C : ");
        for (Integer elem : c) 
            System.out.print(elem + " ");
    }


    static class MyThread extends Thread {
        private int id;
        private int start;
        private int salt;

        public MyThread(int start, int salt) {
            this.id = start;
            this.start = start;
            this.salt = salt;
        }

        public void run() {
            System.out.println("This code is running in a thread " + id);
            for(int i = start;i < c.size();i += salt) {
                int sum = a.get(i) + b.get(i);
                c.set(i, sum);
            }
        }
    }
}