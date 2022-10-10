package model;
import java.util.Objects;

public abstract class Task {
    private String taskId;
    private String description;

    public Task(String taskId, String description) {
        this.taskId = taskId;
        this.description = description;
    }

    public String getTaskId() {
        return taskId;
    }

    public void setTaskId(String taskId) {
        this.taskId = taskId;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public abstract void execute();

    public String toString() {
        return taskId + " " + this.description;
    }

    @Override
    public boolean equals(Object o) {
        if(this == o)
            return true;
        else if(!(o instanceof Task))
            return  false;
        Task task = (Task) o;
        return getTaskId().equals(task.getTaskId())
                && getDescription().equals(task.getDescription());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getTaskId(), getDescription());
    }
}
