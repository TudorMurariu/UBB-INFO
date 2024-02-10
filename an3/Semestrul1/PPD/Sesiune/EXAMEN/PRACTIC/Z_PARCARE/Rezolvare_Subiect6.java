import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Node {
    private String index_car;
    private String type_transaction;
    private Integer amount_cars_in_parking;

    public Node(String index_car, String type_transaction, Integer amount_cars_in_parking) {
        this.index_car = index_car;
        this.type_transaction = type_transaction;
        this.amount_cars_in_parking = amount_cars_in_parking;
    }

    @Override
    public String toString() {
        return "Node{" +
                "index_car='" + index_car + '\'' +
                ", type_transaction='" + type_transaction + '\'' +
                ", amount_cars_in_parking=" + amount_cars_in_parking +
                '}';
    }
}

/*
class FamilyMember extends Thread{

    private Random rand;
    private Lock lockEuro;
    private Lock lockLeu;
    private Bank bank;
    private Condition notFullLeu;
    private Condition notEmptyLeu;
    private Condition notFullEuro;
    private Condition notEmptyEuro;

    public FamilyMember(Lock lockEuro, Condition notFullEuro, Condition notEmptyEuro,
                        Lock lockLeu, Condition notFullLeu, Condition notEmptyLeu,
                        Bank bank) {
        this.lockEuro = lockEuro;
        this.lockLeu = lockLeu;
        this.bank = bank;
        this.notFullLeu = notFullLeu;
        this.notEmptyLeu = notEmptyLeu;
        this.notFullEuro = notFullEuro;
        this.notEmptyEuro = notEmptyEuro;
        this.rand = new Random();
    }

    @Override
    public void run() {

        for(int iteration = 0 ; iteration<10;iteration++) {

            int currency_transaction_rand = rand.nextInt(4);
            int amount_traded = rand.nextInt(1000);

            //System.out.println("thread: " + Thread.currentThread().getName() + " transaction: " + currency_transaction_rand + " ammount: " + amount_traded);

            switch (currency_transaction_rand) {
                case 0:
                    // euro - withdraw
                    lockEuro.lock();
                    //System.out.println("thread: " + Thread.currentThread().getName() + " will lock");
                    try {
                        while (bank.getEuroCurrency() < amount_traded)
                            notEmptyEuro.await();

                        bank.withdrawEuroCurrency(amount_traded);

                        notFullEuro.signal();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    } finally {
                        //System.out.println("thread: " + Thread.currentThread().getName() + " will unlock");
                        lockEuro.unlock();
                    }
                    break;
                case 1:
                    // euro - deposit
                    //System.out.println("thread: " + Thread.currentThread().getName() + " will lock");
                    lockEuro.lock();
                    try {
                        while (bank.getEuroCurrency() < 0)
                            notFullEuro.await();

                        bank.depositEuroCurrency(amount_traded);

                        notEmptyEuro.signal();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    } finally {
                        //System.out.println("thread: " + Thread.currentThread().getName() + " will unlock");
                        lockEuro.unlock();
                    }
                    break;
                case 2:
                    // leu - withdraw
                    lockLeu.lock();
                    try {
                        while (bank.getLeuCurrency() < amount_traded)
                            notEmptyLeu.await();

                        bank.withdrawLeuCurrency(amount_traded);

                        notFullLeu.signal();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    } finally {
                        lockLeu.unlock();
                    }
                    break;
                case 3:
                    // leu - deposit
                    lockLeu.lock();
                    try {
                        while (bank.getLeuCurrency() < 0)
                            notFullLeu.await();

                        bank.depositLeuCurrency(amount_traded);

                        notEmptyLeu.signal();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    } finally {
                        lockLeu.unlock();
                    }
                    break;
                default:
                    //
                    break;
            }

            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}

 */
class EntryCar extends Thread {

    private Random rand;
    private Lock lockParking;
    private Parking parking;
    private Condition notFullParking;
    private Condition notEmptyParking;

    public EntryCar(Lock lockParking, Parking parking,
                    Condition notFullParking, Condition notEmptyParking) {
        this.lockParking = lockParking;
        this.parking = parking;
        this.notFullParking = notFullParking;
        this.notEmptyParking = notEmptyParking;
        this.rand = new Random();
    }

    @Override
    public void run() {

        for (int iteration = 0; iteration < 200; iteration++) {

            int currency_transaction_rand = rand.nextInt(4);
            int amount_traded = rand.nextInt(1000);

            //System.out.println("thread: " + Thread.currentThread().getName() + " transaction: " + currency_transaction_rand + " ammount: " + amount_traded);

            lockParking.lock();
            //System.out.println("thread: " + Thread.currentThread().getName() + " start iteration "+iteration);
            try {
                while (parking.getParkingSlot() == parking.getTotalSlot())
                    notFullParking.await();

                //System.out.println("thread: " + Thread.currentThread().getName() + " will Entry in iteration "+iteration);
                parking.entryCar();

                notEmptyParking.signal();
            } catch (InterruptedException e) {
                e.printStackTrace();
            } finally {
                //System.out.println("thread: " + Thread.currentThread().getName() + " will unlock");
                //System.out.println("thread: " + Thread.currentThread().getName() + " end iteration "+iteration);
                lockParking.unlock();
            }

            try {
                Thread.sleep(20);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}

class ExitCar extends Thread {

    private Random rand;
    private Lock lockParking;
    private Parking parking;
    private Condition notFullParking;
    private Condition notEmptyParking;

    public ExitCar(Lock lockParking, Parking parking,
                   Condition notFullParking, Condition notEmptyParking) {
        this.lockParking = lockParking;
        this.parking = parking;
        this.notFullParking = notFullParking;
        this.notEmptyParking = notEmptyParking;
        this.rand = new Random();
    }

    @Override
    public void run() {

        for (int iteration = 0; iteration < 275; iteration++) {

            int currency_transaction_rand = rand.nextInt(4);
            int amount_traded = rand.nextInt(1000);

            //System.out.println("thread: " + Thread.currentThread().getName() + " transaction: " + currency_transaction_rand + " ammount: " + amount_traded);

            lockParking.lock();
            try {
                while (parking.getParkingSlot() == 0)
                    notEmptyParking.await();

                parking.exitCar();

                notFullParking.signal();
            } catch (InterruptedException e) {
                e.printStackTrace();
            } finally {
                //System.out.println("thread: " + Thread.currentThread().getName() + " will unlock");
                lockParking.unlock();
            }

            try {
                Thread.sleep(15);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}

class IteratorThread extends Thread {

    private Random rand;
    private Lock lockParking;
    private Parking parking;
    private Condition notFullParking;
    private Condition notEmptyParking;

    public IteratorThread(Lock lockParking, Parking parking,
                          Condition notFullParking, Condition notEmptyParking) {
        this.lockParking = lockParking;
        this.parking = parking;
        this.notFullParking = notFullParking;
        this.notEmptyParking = notEmptyParking;
        this.rand = new Random();
    }

    @Override
    public void run() {

        while (true) {

            lockParking.lock();


            System.out.println("no of free slot is: " + parking.getParkingSlot());


            lockParking.unlock();
            try {
                Thread.sleep(5);
            } catch (InterruptedException e) {
                e.printStackTrace();
                break;
            }
        }
    }
}

class Parking {

    private List<Node> transactions = new ArrayList<>();
    private Integer initialSlot;
    private Integer totalSlot;

    private Integer parkingSlot;

    public Parking(Integer initialSlot, Integer totalSlot) {
        this.initialSlot = initialSlot;
        this.totalSlot = totalSlot;
        this.parkingSlot = initialSlot;
    }

    public List<Node> getTransactions() {
        return transactions;
    }

    public void entryCar() {
        this.parkingSlot += 1;
        this.transactions.add(new Node(Thread.currentThread().getName(), "Entry", this.parkingSlot));
        //System.out.println("thread: " + Thread.currentThread().getName() + " Entry  parkingSlot: "+this.parkingSlot);
    }

    public void exitCar() {
        this.parkingSlot -= 1;
        this.transactions.add(new Node(Thread.currentThread().getName(), "Exit", this.parkingSlot));
        //System.out.println("thread: " + Thread.currentThread().getName() + " Exit  parkingSlot: "+this.parkingSlot);
    }

    public Integer getParkingSlot() {
        return parkingSlot;
    }

    public Integer getTotalSlot() {
        return totalSlot;
    }


}

public class Main {

    private static Lock lockParking = new ReentrantLock();
    private static Condition notFullParking = lockParking.newCondition();
    private static Condition notEmptyParking = lockParking.newCondition();

    private static int n;
    private static int p;
    private static int noEntry;
    private static int noExit;
    private static Scanner in = new Scanner(System.in);
    private static Parking parking;


    public static void main(String[] args) {
        //familyMembers = in.nextInt(); // 3
        n = 100;
        p = 25;
        noEntry = 3;
        noExit = 2;

        parking = new Parking(p, n);

        List<EntryCar> entryCars = new ArrayList<>();
        List<ExitCar> exitCars = new ArrayList<>();

        for (int i = 0; i < noEntry; i++) {

            EntryCar entryCar = new EntryCar(lockParking,
                    parking, notFullParking, notEmptyParking);

            entryCar.setName("EntryCar " + i);
            entryCars.add(entryCar);
            System.out.println("EntryCar " + i + " is started");
            entryCar.start();
        }

        for (int i = 0; i < noExit; i++) {

            ExitCar exitCar = new ExitCar(lockParking,
                    parking, notFullParking, notEmptyParking);

            exitCar.setName("ExitCar " + i);
            exitCars.add(exitCar);
            System.out.println("ExitCar " + i + " is started");
            exitCar.start();
        }

        IteratorThread it = new IteratorThread(lockParking,
                parking, notFullParking, notEmptyParking);

        it.start();

        for (int i = 0; i < noExit; i++) {
            try {
                exitCars.get(i).join();
                System.out.println("ExitCar " + i + " is finished");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        for (int i = 0; i < noEntry; i++) {
            try {
                entryCars.get(i).join();
                System.out.println("EntryCar " + i + " is finished");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }


        try {
            System.out.println("it will be interrupted");
            it.interrupt();
            it.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}

NL = lab: 7.96
NT = practic: 10
NE = scris: 7.68
P  = curs: 0.33

NS = ?

medie: 8.46


NE*40+NL*40+NS*5+NT*15]/100  + P/50    iff  NE>4.5 si NL>4.5

[(7.68 * 40) + (7.98 * 40) + (0 * 5) + (10*15) ] / 100 + 0.33/50

[307.2 + 319.2 + 0 + 150 ] / 100 + 0.0066

776.4 / 100 + 0.0066

7.764 + 0.0066

7.7706