package container;

import model.Task;

import static utils.Constants.INITIAL_STACK_SIZE;

public abstract class ContainerSuperclass implements Container {

    protected Task[] tasks;
    protected int size;

    public ContainerSuperclass() {
        size = 0;
        tasks = new Task[INITIAL_STACK_SIZE];
    }

    @Override
    public Task remove() {
        if(size == 0)
            return null;

        return tasks[--size];
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }
}
