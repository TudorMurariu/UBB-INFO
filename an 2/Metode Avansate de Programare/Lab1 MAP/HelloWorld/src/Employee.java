public class Employee {

    public Employee() {}

    public Employee(String name, int age, float salary, String position) {
        this.name = name;
        Age = age;
        Salary = salary;
        Position = position;
    }
    private String name;
    private int Age;
    private float Salary;
    private String Position;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getAge() {
        return Age;
    }

    public void setAge(int age) {
        Age = age;
    }

    public float getSalary() {
        return Salary;
    }

    public void setSalary(float salary) {
        Salary = salary;
    }

    public String getPosition() {
        return Position;
    }

    public void setPosition(String position) {
        Position = position;
    }
}
