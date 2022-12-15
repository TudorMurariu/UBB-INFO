package domain;



import utils.Constants;

import java.time.LocalDate;

public class Nota {

    private Student student;
    private Tema tema;
    private double value;
    private LocalDate date;
    private String profesor;

    public Nota(Student student, Tema tema, double value, LocalDate date, String profesor) {
        this.student = student;
        this.tema = tema;
        this.value = value;
        this.date = date;
        this.profesor = profesor;
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
                "student=" + student +
                ", tema=" + tema +
                ", value=" + value +
                ", date=" + date.format(Constants.DATE_FORMATTER) +
                ", profesor='" + profesor + '\'' +
                '}';
    }
}