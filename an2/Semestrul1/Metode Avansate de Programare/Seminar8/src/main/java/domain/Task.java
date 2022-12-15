package domain;

import java.util.Objects;

public abstract class Task extends Entity<String>{

    private String description;

    public Task(String taskId, String description) {
        super.setId(taskId);
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @Override
    public String toString() {
        return super.getId() + " " + description;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Task)) return false;
        Task task = (Task) o;
        return Objects.equals(super.getId(), task.getId()) &&
                Objects.equals(getDescription(), task.getDescription());
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.getId(), getDescription());
    }

    public abstract void execute();
}
