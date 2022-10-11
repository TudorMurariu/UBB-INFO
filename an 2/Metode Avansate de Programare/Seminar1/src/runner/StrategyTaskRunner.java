package runner;

import container.Container;
import container.Strategy;
import factory.TaskContainerFactory;
import model.Task;

public class StrategyTaskRunner implements TaskRunner {
    private Container container;

    public StrategyTaskRunner(Strategy strategy) {
        this.container = new TaskContainerFactory().createContainer(strategy);
    }
    @Override
    public void executeOneTask() {
        if(!container.isEmpty()) {
            container.remove().execute();
        }
    }

    @Override
    public void executeAll() {
        while(!container.isEmpty()){
            executeOneTask();
        }
    }

    @Override
    public void addTask(Task t) {
        container.add(t);
    }

    @Override
    public boolean hasTask() {
        return !container.isEmpty();
    }
}
