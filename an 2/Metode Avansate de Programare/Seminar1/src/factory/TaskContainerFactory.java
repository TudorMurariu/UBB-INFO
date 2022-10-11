package factory;

import container.Container;
import container.StackContainer;
import container.Strategy;

public class TaskContainerFactory implements Factory {

    public TaskContainerFactory() {
    }
    // TO DO : Singleton


    @Override
    public Container createContainer(Strategy strategy) {
        if(strategy==Strategy.LIFO) {
            return new StackContainer();
        }
        // ...
        return null;
    }
}
