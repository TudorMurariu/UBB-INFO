package container;

import model.Task;

public interface Container {
    Task remove() throws Exception;
    void add(Task task);
    int size();
    boolean isEmpty();
}
