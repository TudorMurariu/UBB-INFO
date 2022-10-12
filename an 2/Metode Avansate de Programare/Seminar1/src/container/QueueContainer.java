package container;

import model.Task;

import java.util.LinkedList;

public class QueueContainer implements Container {

    private LinkedList<Task> tasks;
    private int size;

    public QueueContainer() {
        size = 0;
        tasks = new LinkedList<>();
    }
    @Override
    public Task remove() {
        return tasks.remove();
    }

    @Override
    public void add(Task task) {
        tasks.add(task);
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return tasks.isEmpty();
    }
}
