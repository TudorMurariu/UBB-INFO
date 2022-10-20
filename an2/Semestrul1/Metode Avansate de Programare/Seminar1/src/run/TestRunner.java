package run;

import container.Strategy;
import model.MessageTask;
import model.SortingTask;
import runner.DelayTaskRunner;
import runner.PrinterTaskRunner;
import runner.StrategyTaskRunner;
import sortStuff.SortingStrategy;

import java.time.LocalDateTime;

public class TestRunner {
    public static MessageTask[] getMessages() {
        MessageTask taskLaborator = new MessageTask(
                "1","Seminar", "tema laborator",
                "Florentin", "Razvan", LocalDateTime.now());
        MessageTask taskTema = new MessageTask(
                "2","Laborator", "Solutie",
                "Razvan", "Florentin", LocalDateTime.now());
        MessageTask taskNota = new MessageTask(
                "3","Nota Lab", "10",
                "Florentin", "Razvan", LocalDateTime.now());
        return new MessageTask[]{
                taskLaborator, taskTema, taskNota
        };
    }

    public static void run(Strategy strategy, SortingStrategy sortingStrategy) {

        int[] arr = {1,5,7,9,0,3,3,2};
        SortingTask sortingTask = new SortingTask("12343", "Task1", arr, sortingStrategy);

        System.out.println("Array initial");
        for(int x : sortingTask.array)
            System.out.print(x + " ");
        System.out.println();

        sortingTask.sort();

        System.out.println("Array sortat");
        for(int x : sortingTask.array)
            System.out.print(x + " ");
        System.out.println();

        /*StrategyTaskRunner runner = new StrategyTaskRunner(Strategy.LIFO);
        MessageTask[] messages = getMessages();
        runner.addTask(messages[0]);
        runner.addTask(messages[1]);
        runner.addTask(messages[2]);
        runner.executeAll();*/

        StrategyTaskRunner runner = new StrategyTaskRunner(strategy);
        DelayTaskRunner printer = new DelayTaskRunner(runner);
        //PrinterTaskRunner printer = new PrinterTaskRunner(runner);

        MessageTask[] messages = getMessages();
        printer.addTask(messages[0]);
        printer.addTask(messages[1]);
        printer.addTask(messages[2]);
        printer.executeAll();


        /*MessageTask[] messages = getMessages();
        for(MessageTask message : messages) {
            message.execute();
        }*/
    }
}
