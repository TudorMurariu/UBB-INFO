package container;

import model.Task;

import java.util.LinkedList;

public class QueueContainer extends ContainerSuperclass {
    public QueueContainer() {
        super();
    }

    @Override
    public void add(Task task) {
        if(tasks.length == size) {
            Task[] t = new Task[tasks.length*2];
            System.arraycopy(tasks, 0, t, 1, tasks.length);
            tasks = t;
        }
        else {
            for(int i = size;i >= 0;--i)
                tasks[i+1] = tasks[i];
        }
        tasks[0] = task;
        size++;
    }
}
