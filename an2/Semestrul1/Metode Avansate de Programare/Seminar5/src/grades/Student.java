package grades;

public class Student extends Entity<Long> {
    private int group;
    private String name;

    public Student(int group, String name) {
        this.group = group;
        this.name = name;
    }

    public int getGroup() {
        return group;
    }

    public void setGroup(int group) {
        this.group = group;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "Student{" +
                "group=" + group +
                ", name='" + name + '\'' +
                '}';
    }
}
