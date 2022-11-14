package stream;

import grades.Nota;
import grades.Student;
import grades.Tema;

import java.util.*;

public class Stream {
    public static void main(String[] args) throws Exception {
        double medie =  getMedieTema(getNote(), new Tema("desc1", "1"));
        System.out.println("Medie: " + medie);

        Tema resTema = temaMax(getNote(), Double::compare);
        System.out.println(resTema);

        Tema resTema2 = temaMax(getNote(), (o1, o2) -> Double.compare(o2, o1));
        System.out.println(resTema2 + " " + getMedieTema(getNote(), resTema2));
    }

    public static void main1() {
        List<String> stringList =
                Arrays.asList("adsd", "fsdf", "afesa", "fgh", "ab");

        String rez = stringList.stream()
                .filter(x -> x.startsWith("a"))
                .map(String::toUpperCase)
                .reduce("", (x,y) -> x + y);

        System.out.println("A:\n" + rez);

        System.out.println("B:\n" );

        stringList.stream()
                .filter(x -> x.startsWith("a"))
                .map(String::toUpperCase)
                .forEach(System.out::println);

        System.out.println("C:\n" );

        Optional<String> stringOptional = stringList.stream()
                .filter(x -> x.startsWith("a"))
                .map(String::toUpperCase)
                .reduce((x,y) -> x + y);

        if(!stringOptional.isEmpty())
            System.out.println(stringOptional.get());
        stringOptional.ifPresent(System.out::println);
    }

    public static List<Nota> getNote() {
        List<Nota> note = new ArrayList<>();
        Tema tema1 = new Tema("desc1", "1");
        Tema tema2 = new Tema("desc2", "2");
        Tema tema3 = new Tema("desc3", "3");
        Tema tema4 = new Tema("desc4", "4");
        note.add(new Nota("Prof 1", new Student(225, "Student 1"), tema1, 8.6));
        note.add(new Nota("Prof 1", new Student(225, "Student 2"), tema1, 9));
        note.add(new Nota("Prof 1", new Student(225, "Student 3"), tema1, 7.2));
        note.add(new Nota("Prof 1", new Student(225, "Student 1"), tema1, 4));
        note.add(new Nota("Prof 2", new Student(225, "Student 3"), tema2, 8));
        note.add(new Nota("Prof 2", new Student(225, "Student 1"), tema2, 5));
        note.add(new Nota("Prof 3", new Student(225, "Student 2"), tema3, 6.40));
        note.add(new Nota("Prof 3", new Student(225, "Student 1"), tema3, 8.6));
        note.add(new Nota("Prof 3", new Student(225, "Student 3"), tema3, 10));
        note.add(new Nota("Prof 4", new Student(225, "Student 1"), tema4, 8.5));
        note.add(new Nota("Prof 4", new Student(225, "Student 1"), tema4, 10));
        return note;
    }

    public static double getMedieTema(List<Nota> grades, Tema tema) {
        return grades.stream()
                .filter(x -> x.getTema().equals(tema)).mapToDouble(Nota::getValue).average().orElse(0);
    }

//    public static Tema getMax

    private static Tema temaMax(List<Nota> listaNote, Comparator<Double> comparator) throws Exception {
        Optional<Double> mediaMaxOpt = listaNote.stream()
                .map(nota -> getMedieTema(listaNote, nota.getTema()))
                .max(comparator); // Double::compareTo

        System.out.println(mediaMaxOpt.get());

        var medieMax1 = listaNote.stream()
                .map(nota -> getMedieTema(listaNote, nota.getTema()))
                .reduce(0d, (x, y) -> {
                    if (x > y)
                        return x;
                    return y;
                });
//        System.out.println(medieMax1);

        if(mediaMaxOpt.isEmpty())
            throw new Exception("");

        double medieMax = mediaMaxOpt.get();

        var list = listaNote.stream()
                .map(Nota::getTema)
                .filter(tema -> getMedieTema(listaNote, tema) == medieMax).toList();
//        System.out.println(list);
        return list.get(0);
    }
}
