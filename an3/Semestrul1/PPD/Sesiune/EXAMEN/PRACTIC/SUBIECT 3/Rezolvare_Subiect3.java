import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Node {
    private String index_user;
    private String type_currency;
    private String type_transaction;
    private Integer amount_traded;
    private Integer amount_current;

    public Node(String index_user, String type_currency,
               String type_transaction, Integer amount_traded,
               Integer amount_current) {
        this.index_user = index_user;
        this.type_currency = type_currency;
        this.type_transaction = type_transaction;
        this.amount_traded = amount_traded;
        this.amount_current = amount_current;
    }

    @Override
    public String toString() {
        return "Nod{" +
                "index_user=" + index_user +
                ", type_currency='" + type_currency + '\'' +
                ", type_transaction='" + type_transaction + '\'' +
                ", amount_traded=" + amount_traded +
                ", amount_current=" + amount_current +
                '}';
    }
}

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

class IteratorThread extends Thread {

    private Random rand;
    private Lock lockEuro;
    private Lock lockLeu;
    private Bank bank;
    private Condition notFullLeu;
    private Condition notEmptyLeu;
    private Condition notFullEuro;
    private Condition notEmptyEuro;

    public IteratorThread(Lock lockEuro, Condition notFullEuro, Condition notEmptyEuro,
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

        while(true){

            lockEuro.lock();
            lockLeu.lock();

            for (int i = Math.max(0, bank.getTransactions().size()-5); i < bank.getTransactions().size(); i++) {
                System.out.println(bank.getTransactions().get(i));
                System.out.println("index: "+i);
            }

            lockEuro.unlock();
            lockLeu.unlock();
            System.out.println("iterator");

            try {
                Thread.sleep(300);
            } catch (InterruptedException e) {
                //e.printStackTrace();
                break;
            }

        }

    }
}

class Bank{

    private List<Node> transactions = new ArrayList<>();
    private Integer bankEuroAmount;
    private Integer bankLeuAmount;

    public Bank() {
        bankEuroAmount = 99990;
        bankLeuAmount = 99990;
    }

    public List<Node> getTransactions() {
        return transactions;
    }

    public void withdrawEuroCurrency(Integer euroAmount) {
        this.bankEuroAmount -= euroAmount;
        this.transactions.add(new Node(Thread.currentThread().getName(), "Euro", "withdraw", euroAmount, this.bankEuroAmount));
/*
        System.out.println("thread: "+Thread.currentThread().getName()+
                " typeCurrency: Euro"+
                " typeTransaction: withdraw"+
                " amountTrade: "+euroAmount+
                " amountNow: "+this.bankEuroAmount);

 */


    }

    public void depositEuroCurrency(Integer euroAmount) {
        this.bankEuroAmount += euroAmount;
        this.transactions.add(new Node(Thread.currentThread().getName(), "Euro", "deposit", euroAmount, this.bankEuroAmount));
/*
        System.out.println("thread: "+Thread.currentThread().getName()+
                " typeCurrency: Euro"+
                " typeTransaction: deposit"+
                " amountTrade: "+euroAmount+
                " amountNow: "+this.bankEuroAmount);

 */


    }

    public Integer getEuroCurrency() {
        return this.bankEuroAmount;
    }

    public void withdrawLeuCurrency(Integer leuAmount) {
        this.bankLeuAmount -= leuAmount;
        this.transactions.add(new Node(Thread.currentThread().getName(), "Leu", "withdraw", leuAmount, this.bankLeuAmount));
/*
        System.out.println("thread: "+Thread.currentThread().getName()+
                " typeCurrency: Leu"+
                " typeTransaction: withdraw"+
                " amountTrade: "+leuAmount+
                " amountNow: "+this.bankLeuAmount);

 */

    }

    public void depositLeuCurrency(Integer leuAmount) {
        this.bankLeuAmount += leuAmount;
        this.transactions.add(new Node(Thread.currentThread().getName(), "Leu", "deposit", leuAmount, this.bankLeuAmount));
/*
        System.out.println("thread: "+Thread.currentThread().getName()+
                " typeCurrency: Leu"+
                " typeTransaction: deposit"+
                " amountTrade: "+leuAmount+
                " amountNow: "+this.bankLeuAmount);

 */

    }

    public Integer getLeuCurrency() {
        return this.bankLeuAmount;
    }
}




public class Main {

    private static Lock lockEuro = new ReentrantLock();
    private static Lock lockLeu = new ReentrantLock();
    private static Condition notFullLeu = lockLeu.newCondition();
    private static Condition notEmptyLeu = lockLeu.newCondition();
    private static Condition notFullEuro = lockEuro.newCondition();
    private static Condition notEmptyEuro = lockEuro.newCondition();
    private static int familyMembersNo;
    private static Scanner in = new Scanner(System.in);
    private static Bank bank = new Bank();


    public static void main(String[] args) {
        //familyMembers = in.nextInt(); // 3
        familyMembersNo = 3; // 20

        List<FamilyMember> familyMembers = new ArrayList<>();

        for (int i = 0; i < familyMembersNo; i++) {
            FamilyMember familyMember = new FamilyMember(lockEuro, notFullEuro, notEmptyEuro,
                    lockLeu, notFullLeu, notEmptyLeu,
                    bank);
            familyMember.setName("Thread "+i);
            familyMembers.add(familyMember);
            System.out.println("thread "+i+" is started");
            familyMember.start();
        }

        IteratorThread it = new IteratorThread(lockEuro, notFullEuro, notEmptyEuro,
                lockLeu, notFullLeu, notEmptyLeu,
                bank);

        it.start();

        for (int i = 0; i < familyMembersNo; i++) {
            try {
                familyMembers.get(i).join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        try {
            it.interrupt();
            it.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }


    }
}
