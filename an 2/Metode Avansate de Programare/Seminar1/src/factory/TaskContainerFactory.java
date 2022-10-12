package factory;

import container.Container;
import container.QueueContainer;
import container.StackContainer;
import container.Strategy;

public final class TaskContainerFactory implements Factory {

    private static TaskContainerFactory INSTANCE;

    private TaskContainerFactory() {}

    public synchronized static TaskContainerFactory getInstance() {
        if(INSTANCE == null)
            INSTANCE = new TaskContainerFactory();
        
        return INSTANCE;
    }

    @Override
    public Container createContainer(Strategy strategy) {
        if(strategy==Strategy.LIFO)
            return new StackContainer();

        return new QueueContainer();
    }
}
