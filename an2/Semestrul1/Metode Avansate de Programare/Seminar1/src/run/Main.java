package run;


import container.Strategy;
import runner.StrategyTaskRunner;
import sortStuff.SortingStrategy;

public class Main {
    public static void main(String[] args) {
        Strategy strategy = Strategy.valueOf(args[0]);
        SortingStrategy sortingStrategy = SortingStrategy.valueOf(args[1]);
        TestRunner.run(strategy, sortingStrategy);
    }
}