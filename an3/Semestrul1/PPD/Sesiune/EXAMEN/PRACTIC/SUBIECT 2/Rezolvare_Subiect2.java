import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Nod {
    private long ID;
    private boolean tipOperatie;// 0 for get / 1 for put
    private int nrObiectePeBanda;

    public Nod(long ID, boolean tipOperatie, int nrObiectePeBanda) {
        this.ID = ID;
        this.tipOperatie = tipOperatie;
        this.nrObiectePeBanda = nrObiectePeBanda;
    }

    @Override
    public String toString() {
        return "Nod{" +
                "ID=" + ID +
                ", tipOperatie=" + tipOperatie +
                ", nrObiectePeBanda=" + nrObiectePeBanda +
                '}';
    }
}

class Producer extends Thread {
    private static Lock lock;
    private static SynchronizedQueue banda;

    public Producer(Lock newLock, SynchronizedQueue newBanda){
        lock = newLock;
        banda = newBanda;
    }

    @Override
    public void run() {
        int i = 100;
        while (i > 0) {

            lock.lock();

            // verificam sa nu fie banda prea plina
            while(banda.isTooFullToDeposit) {
                synchronized (banda.tooFull) {
                    try {
                        banda.tooFull.wait();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }

            // adaugam 4 obiecte in banda

            banda.banda.add(0, 11111);
            banda.banda.add(0, 11111);
            banda.banda.add(0, 11111);
            banda.banda.add(0, 11111);

            // insert in operatii
            banda.operatii.add(new Nod(currentThread().getId(), true, banda.banda.size()));


            // TODO: verificat ca n - size >= 4 (mai putem depune in coada obiecte)
            if (banda.banda.size() > banda.capacity - 4) {
                banda.isTooFullToDeposit = true;
            }

            // notificam consumatorii ca am produs
            banda.isTooEmptyToWithdraw = false;
            synchronized (banda.tooEmpty){
                banda.tooEmpty.notify();
            }

            lock.unlock();

            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            // trecem peste o iteratie
            i--;
        }
    }

}


class Consumer extends Thread {
    private static Lock lock;
    private static SynchronizedQueue banda;

    public Consumer(Lock newLock, SynchronizedQueue newBanda){
        lock = newLock;
        banda = newBanda;
    }

    @Override
    public void run() {
        int i = 100;
        while (i > 0) {

            lock.lock();

            // verificam sa nu fie banda goala
            while(banda.isTooEmptyToWithdraw) {
                synchronized (banda.tooEmpty) {
                    try {
                        banda.tooEmpty.wait();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }

            // luam 3 obiecte de pe banda
            banda.banda.remove(banda.banda.size()-1);
            banda.banda.remove(banda.banda.size()-1);
            banda.banda.remove(banda.banda.size()-1);

            // insert in operatii
            banda.operatii.add(new Nod(currentThread().getId(), false, banda.banda.size()));


            // verificam daca banda este prea goala acum
            // TODO: verificat ca size >= 3 (mai putem retrage din coada obiecte)
            if (banda.banda.size() < 3){
                banda.isTooEmptyToWithdraw = true;
            }


            // notificam producatorii ca am luat doar daca avem spatiu de 4+ iteme
            if (banda.banda.size() < banda.capacity - 4) {
                banda.isTooFullToDeposit = false;
                synchronized (banda.tooFull) {
                    banda.tooFull.notify();
                }
            }

            lock.unlock();

            try {
                Thread.sleep(8);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            // trecem peste o iteratie
            i--;
        }
    }

}


class IteratorThread extends Thread {
    private static Lock lock;
    private static SynchronizedQueue banda;

    // cand am citit size-ul pentru operatii, am vazut size-ul sau. Daca acesta nu a mai crescut, game over.
    private static int lastState = -1;

    public IteratorThread(Lock newLock, SynchronizedQueue newBanda) {
        lock = newLock;
        banda = newBanda;
    }

    @Override
    public void run() {
        while (true) {
            lock.lock();

            banda.operatii.forEach(System.out::println);

            // check state not changed
            if (banda.operatii.size() == lastState) {
                lock.unlock();
                return;
            }

            lastState = banda.operatii.size();

            lock.unlock();

            try {
                Thread.sleep(20);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

}



class SynchronizedQueue {
    public List<Nod> operatii = new ArrayList<>();
    public List<Integer> banda = new ArrayList<>();
    public int capacity;
    public Boolean isTooEmptyToWithdraw = true;
    public final Object tooEmpty = new Object();
    public Boolean isTooFullToDeposit = false;
    public final Object tooFull = new Object();

    public SynchronizedQueue(int n){
        capacity = n;
    }

}

public class Main {
    private static SynchronizedQueue banda;
    private static int p;
    private static int n;
    private static int c;
    private static Lock lock = new ReentrantLock();
    private static Scanner in = new Scanner(System.in);

    public static void main(String[] args) {
        n = in.nextInt(); // 20
        p = in.nextInt(); // 3
        c = in.nextInt(); // 2

        banda = new SynchronizedQueue(n);

        List<Producer> producers = new ArrayList<>();
        List<Consumer> consumers = new ArrayList<>();
        IteratorThread iteratorThread = new IteratorThread(lock, banda);

        for (int i = 0; i < p; i++) {
            Producer producer = new Producer(lock, banda);
            producers.add(producer);
            producer.start();
        }

        iteratorThread.start();

        for (int i = 0; i < p; i++) {
            Consumer consumer = new Consumer(lock, banda);
            consumers.add(consumer);
            consumer.start();
        }

        for (int i = 0; i < p; i++) {
            try {
                producers.get(i).join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        for (int i = 0; i < c; i++) {
            try {
                consumers.get(i).join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        try {
            iteratorThread.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
