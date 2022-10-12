package container;

import model.Task;

import java.util.LinkedList;

public class QueueContainer extends ContainerSuperclass {

    private int inc;
    public QueueContainer() {
        super();
        inc = 0;
    }
    @Override
    public Task remove() {
        if(inc >= size)
            return null;

        return tasks[inc--];
    }

    @Override
    public void add(Task task) {
        tasks[size++] = task;
    }

    @Override
    public int size() {
        return size - inc;
    }

    @Override
    public boolean isEmpty() {
        return size == inc;
    }
}
