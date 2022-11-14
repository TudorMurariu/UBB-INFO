package grades;

public class Nota {

    private String profesor;

    private Student student;

    private Tema tema;

    private double value;

    public Nota(String profesor, Student student, Tema tema, double value) {
        this.profesor = profesor;
        this.student = student;
        this.tema = tema;
        this.value = value;
    }

    public String getProfesor() {
        return profesor;
    }

    public void setProfesor(String profesor) {
        this.profesor = profesor;
    }

    public Student getStudent() {
        return student;
    }

    public void setStudent(Student student) {
        this.student = student;
    }

    public Tema getTema() {
        return tema;
    }

    public void setTema(Tema tema) {
        this.tema = tema;
    }

    public double getValue() {
        return value;
    }

    public void setValue(double value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return "Nota{" +
                "profesor='" + profesor + '\'' +
                ", student=" + student +
                ", tema=" + tema +
                ", value=" + value +
                '}';
    }
}
