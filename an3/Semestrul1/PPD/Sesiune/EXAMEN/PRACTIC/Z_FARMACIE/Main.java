package com.company;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;


class Main {
    static class Inregistrare {
        private long nr_Factura;
        private int valoare;
        private long codReteta;

        public Inregistrare(long nr_Factura, int valoare, long codReteta) {
            this.nr_Factura = nr_Factura;
            this.valoare = valoare;
            this.codReteta = codReteta;
        }

        @Override
        public String toString() {
            return "Inregistrare{" +
                    "nr_Factura=" + nr_Factura +
                    ", valoare=" + valoare +
                    ", codReteta=" + codReteta +
                    '}';
        }
    }

    static class Reteta {
        private long codReteta;
        private int nrMedicamente;
        private List<Long> listaMedicamente;

        public Reteta(long codReteta, int nrMedicamente, List<Long> listaMedicamente) {
            this.codReteta = codReteta;
            this.nrMedicamente = nrMedicamente;
            this.listaMedicamente = listaMedicamente;
        }

        @Override
        public String toString() {
            return "Reteta{" +
                    "codReteta=" + codReteta +
                    ", nrMedicamente=" + nrMedicamente +
                    ", listaMedicamente=" + listaMedicamente +
                    '}';
        }
    }

    static class Pacient implements Runnable {
        private BlockingQueue<Reteta> banda;

        public Pacient(BlockingQueue<Reteta> banda) {
            this.banda = banda;
        }

        public void Pune() {
            synchronized (banda) {
                Random rand = new Random();
                int cod;
                synchronized (this){
                    cod = codRetete++;
                }
                int nrMedicamente = rand.nextInt(5) + 1;
                List<Long> medicamente = new ArrayList<>();
                for (int i = 0; i < nrMedicamente; i++) {
                    long codMedicament = rand.nextInt(30) + 1;
                    medicamente.add(codMedicament);
                }
                Reteta reteta = new Reteta(cod, nrMedicamente, medicamente);
                banda.add(reteta);
                banda.notifyAll();
                System.out.println("Reteta nr " + cod + " a fost generata de catre pacientul " + Thread.currentThread().getName());
            }
        }

        @Override
        public void run() {
            Pune();
            synchronized (this) {
                synchronized (banda) {
                    pacientiDone++;
                    if (pacientiDone == N) {
                        banda.notifyAll();
                    }
                }
            }
        }
    }


    static class Farmacist extends Thread {
        private BlockingQueue<Reteta> banda;
        private BlockingQueue<Inregistrare> comenzi;

        public Farmacist(BlockingQueue<Reteta> newBanda, BlockingQueue<Inregistrare> newOperatii) {
            banda = newBanda;
            comenzi = newOperatii;
        }

        public void Preia() {
            while (true) {
                synchronized (banda) {
                    // verificam sa nu fie banda prea goala
                    while (banda.isEmpty() && pacientiDone < N) {
                        try {
                            banda.wait();
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }
                    if (banda.isEmpty() && pacientiDone == N) {
                        break;
                    } else {
                        if (!banda.isEmpty()) {
                            Reteta reteta = banda.poll();
                            System.out.println("Reteta nr " + reteta.codReteta + " a fost preluata de catre farmacistul " + Thread.currentThread().getName());
                            if (reteta != null) {
                                synchronized (comenzi) {
                                    int sum = 0;
                                    for (int i = 0; i < reteta.nrMedicamente; i++) {
                                        long codMedicament = reteta.listaMedicamente.get(i);
                                        try {
                                            File myObj = new File("Medicamente.txt");
                                            Scanner myReader = new Scanner(myObj);
                                            String linie = "";
                                            int j = 0;
                                            while (myReader.hasNext() && j < codMedicament) {
                                                linie = myReader.nextLine();
                                                j++;
                                            }
                                            sum += Integer.parseInt(linie.split(" ")[1]);
                                        } catch (FileNotFoundException e) {
                                            System.out.println("An error occurred.");
                                            e.printStackTrace();
                                        }
                                    }
                                    System.out.println("Reteta nr " + reteta.codReteta + " a fost pregatita si pretul este de " + sum);
                                    Inregistrare inregistrare = new Inregistrare(comenzi.size() + 1, sum, reteta.codReteta);
                                    comenzi.add(inregistrare);
                                    comenzi.notify();
                                    System.out.println("Reteta nr " + reteta.codReteta + " a fost adaugata in coada de la casierie ");
                                }
                            }
                        }
                    }
                }
            }
        }

        @Override
        public void run() {
            Preia();
            synchronized (this) {
                synchronized (comenzi) {
                    farmacistiDone++;
                    if (farmacistiDone == M) {
                        comenzi.notify();
                    }
                }
            }
        }
    }

    static class Casier implements Runnable {
        private BlockingQueue<Inregistrare> comenzi;

        public Casier(BlockingQueue<Inregistrare> banda) {
            this.comenzi = banda;
        }

        @Override
        public void run() {
            while (true) {
                synchronized (comenzi) {
                    while (comenzi.isEmpty() && farmacistiDone < M) {
                        try {
                            comenzi.wait();
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }
                    if (comenzi.isEmpty() && farmacistiDone == M) {
                        return;
                    } else {
                        if (!comenzi.isEmpty()) {
                            Inregistrare inregistrare = comenzi.poll();
                            System.out.println("Reteta nr " + inregistrare.codReteta + " a fost preluata");
                            try (BufferedWriter bw = new BufferedWriter(new FileWriter("Inregistrari.txt"))) {
                                bw.write(inregistrare.nr_Factura + " " + inregistrare.valoare + " " + inregistrare.codReteta);
                                bw.newLine();
                                System.out.println("Reteta nr " + inregistrare.codReteta + " a fost platita");
                            } catch (IOException e) {
                                e.printStackTrace();
                            }
                        }
                    }
                }
            }
        }
    }

    private static void genereazaMedicamente() {
        Random rand = new Random();
        try (BufferedWriter bw = new BufferedWriter(new FileWriter("Medicamente.txt"))) {
            for (int i = 1; i <= 30; i++) {
                int cod = rand.nextInt(100) + 1;
                bw.write(i + " " + cod);
                bw.newLine();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static BlockingQueue<Reteta> banda;
    private static BlockingQueue<Inregistrare> comenzi;
    private static int N = 100;
    private static int M = 4;
    protected static int pacientiDone = 0;
    protected static int farmacistiDone = 0;
    protected static int codRetete = 1;

    public static void main(String[] args) {
        banda = new LinkedBlockingQueue<>();
        comenzi = new LinkedBlockingQueue<>();

        List<Thread> pacienti = new ArrayList<>();
        List<Thread> farmacisti = new ArrayList<>();

        genereazaMedicamente();

        Pacient pacient = new Pacient(banda);
        Farmacist farmacist = new Farmacist(banda, comenzi);
        Casier casier = new Casier(comenzi);

        Thread casierWorker = new Thread(casier);
        for (int i = 0; i < N; i++) {
            Thread pacientWorker = new Thread(pacient);
            pacienti.add(pacientWorker);
        }

        for (int i = 0; i < M; i++) {
            Thread farmacistWorker = new Thread(farmacist);
            farmacisti.add(farmacistWorker);
        }

        casierWorker.start();
        pacienti.forEach(Thread::start);
        farmacisti.forEach(Thread::start);

        pacienti.forEach(pr -> {
            try {
                pr.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        });

        farmacisti.forEach(cn -> {
            try {
                cn.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        });

        try {
            casierWorker.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
