package factory;

import container.Container;
import container.Strategy;

public interface Factory {
    Container createContainer(Strategy strategy);
}
