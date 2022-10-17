package models;

import java.util.Objects;

public class Student {
    private String nume;
    private float media;

    public String getNume() {
        return nume;
    }

    public void setNume(String nume) {
        this.nume = nume;
    }

    public float getMedia() {
        return media;
    }

    public void setMedia(float media) {
        this.media = media;
    }

    public Student(String nume, float media) {
        this.nume = nume;
        this.media = media;
    }

    @Override
    public String toString() {
        return nume + " " + String.valueOf(media);
    }

    @Override
    public int hashCode() {
        return Objects.hash(nume, media);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        else if (o == null || getClass() != o.getClass())
            return false;
        Student student = (Student) o;
        return Float.compare(student.media, media) == 0 && Objects.equals(nume, student.nume);
    }

    public int getMediaRotunjita() {
        return Math.round(this.media);
    }
}
