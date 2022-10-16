package run;


import container.Strategy;
import runner.StrategyTaskRunner;

public class Main {
    public static void main(String[] args) {
        Strategy strategy = Strategy.valueOf(args[0]);
        TestRunner.run(strategy);
    }
}