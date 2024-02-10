package com.company;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

enum Transaction {
    ENTRY, EXIT;

    public String ToString() {
        if (this == ENTRY) {
            return "ENTRY";
        }
        if (this == EXIT) {
            return "EXIT";
        }
        return "";
    }
}

class Node {
    private final String carIndex;
    private final Transaction transaction;
    private final Integer amountOfCarsParked;

    public Node(String carIndex, Transaction transaction, Integer amountOfCarsParked) {
        this.carIndex = carIndex;
        this.transaction = transaction;
        this.amountOfCarsParked = amountOfCarsParked;
    }

    @Override
    public String toString() {
        return "Node {" +
                "car = '" + carIndex + '\'' +
                ", transaction = '" + transaction.ToString() + '\'' +
                ", amountOfCarsParked = " + amountOfCarsParked +
                " }";
    }
}

class EntryCar extends Thread {
    private final Random rand;
    private final Lock lockParking;
    private final Parking parking;
    private final Condition notFullParking;
    private final Condition notEmptyParking;

    public EntryCar(Lock lockParking, Parking parking, Condition notFullParking, Condition notEmptyParking) {
        this.lockParking = lockParking;
        this.parking = parking;
        this.notFullParking = notFullParking;
        this.notEmptyParking = notEmptyParking;
        this.rand = new Random();
    }

    @Override
    public void run() {
        for (int iteration = 0; iteration < 200; iteration++) {
            lockParking.lock();
            try {
                while (parking.getParkedSlots().equals(parking.getTotalSlot())) {
                    notFullParking.await();
                }
                parking.entryCar();
                notEmptyParking.signal();
            } catch (InterruptedException e) {
                e.printStackTrace();
            } finally {
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

    private final Random rand;
    private final Lock lockParking;
    private final Parking parking;
    private final Condition notFullParking;
    private final Condition notEmptyParking;

    public ExitCar(Lock lockParking, Parking parking, Condition notFullParking, Condition notEmptyParking) {
        this.lockParking = lockParking;
        this.parking = parking;
        this.notFullParking = notFullParking;
        this.notEmptyParking = notEmptyParking;
        this.rand = new Random();
    }

    @Override
    public void run() {
        for (int iteration = 0; iteration < 275; iteration++) {
            lockParking.lock();
            try {
                while (parking.getParkedSlots() == 0) {
                    notEmptyParking.await();
                }
                parking.exitCar();
                notFullParking.signal();
            } catch (InterruptedException e) {
                e.printStackTrace();
            } finally {
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
    private final Random rand;
    private final Lock lockParking;
    private final Parking parking;
    private final Condition notFullParking;
    private final Condition notEmptyParking;

    public IteratorThread(Lock lockParking, Parking parking, Condition notFullParking, Condition notEmptyParking) {
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
            System.out.println("Number of free slots: " + parking.getParkedSlots());
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
    private final List<Node> transactions = new ArrayList<>();
    private final Integer initialSlot;
    private final Integer totalSlot;
    private Integer parkedSlots;

    public Parking(Integer initialSlot, Integer totalSlot) {
        this.initialSlot = initialSlot;
        this.totalSlot = totalSlot;
        this.parkedSlots = initialSlot;
    }

    public List<Node> getTransactions() {
        return this.transactions;
    }

    public void entryCar() {
        this.parkedSlots += 1;
        this.transactions.add(new Node(Thread.currentThread().getName(), Transaction.ENTRY, this.parkedSlots));
    }

    public void exitCar() {
        this.parkedSlots -= 1;
        this.transactions.add(new Node(Thread.currentThread().getName(), Transaction.EXIT, this.parkedSlots));
    }

    public Integer getParkedSlots() {
        return this.parkedSlots;
    }

    public Integer getTotalSlot() {
        return this.totalSlot;
    }
}

public class Main {
    private static final Lock lockParking = new ReentrantLock();
    private static final Condition notFullParking = lockParking.newCondition();
    private static final Condition notEmptyParking = lockParking.newCondition();

    public static void main(String[] args) {
        int n = 100;
        int p = 25;
        int noEntries = 3;
        int noExits = 2;

        Parking parking = new Parking(p, n);

        List<EntryCar> entryCars = new ArrayList<>();
        List<ExitCar> exitCars = new ArrayList<>();

        for (int i = 0; i < noEntries; i++) {
            EntryCar entryCar = new EntryCar(lockParking, parking, notFullParking, notEmptyParking);
            entryCar.setName("EntryCar " + i);
            entryCars.add(entryCar);
            System.out.println("EntryCar " + i + " is entering...");
            entryCar.start();
        }

        for (int i = 0; i < noExits; i++) {
            ExitCar exitCar = new ExitCar(lockParking, parking, notFullParking, notEmptyParking);
            exitCar.setName("ExitCar " + i);
            exitCars.add(exitCar);
            System.out.println("ExitCar " + i + " is exiting...");
            exitCar.start();
        }

        IteratorThread it = new IteratorThread(lockParking, parking, notFullParking, notEmptyParking);
        it.start();

        for (int i = 0; i < noExits; i++) {
            try {
                exitCars.get(i).join();
                System.out.println("ExitCar " + i + " finished!");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        for (int i = 0; i < noEntries; i++) {
            try {
                entryCars.get(i).join();
                System.out.println("EntryCar " + i + " finished!");
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        try {
            System.out.println("Interrupting iterator thread...");
            it.interrupt();
            it.join();
        } catch (InterruptedException e) {
            System.out.println("Interruption caught!");
        }
    }
}