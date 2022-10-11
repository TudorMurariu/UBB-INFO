package model;

import java.util.Objects;

public class Task {
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

    public void execute() {

    }

    @Override
    public String toString(){
        return taskId+" " + description;
    }

    @Override
    public boolean equals(Object obj) {
        if(this==obj) {
            return true;
        }
        if(!(obj instanceof Task))
            return false;

        Task task = (Task)obj;

        return Objects.equals(getTaskId(), task.getTaskId()) &&
                Objects.equals(getDescription(), task.getDescription());
    }

    @Override
    public int hashCode(){
        return Objects.hash(getTaskId(), getDescription());
    }
}
