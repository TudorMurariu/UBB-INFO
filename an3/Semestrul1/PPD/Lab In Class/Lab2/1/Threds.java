class Threds {

    public static void main(String[] args) {
        int p = 4;
        Thread[] threads = new Thread[p];
        for(int i=0;i<p;++i) {
            threads[i] = new MyThread(i);
            threads[i].start();
        }
        
        for(int i=0;i<p;++i) {
            try {
                threads[i].join();
            } catch(InterruptedException e) {
                e.printStackTrace();
            }
        }
    }


    static class MyThread extends Thread {
        private int id;

        public MyThread(int id) {
            this.id = id;
        }

        public void run() {
            System.out.println("This code is running in a thread " + id);
          }
    }
}