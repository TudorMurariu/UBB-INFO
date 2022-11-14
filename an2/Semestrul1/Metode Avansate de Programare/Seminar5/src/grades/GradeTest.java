package grades;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public class GradeTest {
    public static void main(String[] args) {

        Student s1 = new Student(225, "Andrei");
        Student s2 = new Student(225, "Matei");
        Student s3 = new Student(226, "Ema");
        Student s4 = new Student(226, "Larisa");
        Student s5 = new Student(225, "Gabi");

        s1.setId(1L);
        s2.setId(2L);
        s3.setId(3L);
        s4.setId(4L);
        s5.setId(5L);


        List<Student> studentList = Arrays.asList(s1, s2 ,s3 ,s4 ,s5);

        Tema t1 = new Tema("Tema MAP", "1");
        Tema t2 = new Tema("Tema ASC", "2");
        Tema t3 = new Tema("Tema Algebra", "3");
        Tema t4 = new Tema("Tema OOP", "4");
        Tema t5 = new Tema("Tema SDA", "5");

        List<Tema> temaList = Arrays.asList(t1, t2, t3, t4, t5);

        Nota n1 = new Nota("Alex", s1, t5, 9);
        Nota n2 = new Nota("Antonin", s2, t2, 8);
        Nota n3 = new Nota("Nicu", s3, t3, 5);
        Nota n4 = new Nota("Antonin", s4, t4, 7);
        Nota n5 = new Nota("Antonin", s5, t1, 10);

        List<Nota> notaList = Arrays.asList(n1, n2, n3, n4, n5);

        report1(notaList);
        report2(notaList);
    }

    private static void report1(List<Nota> l) {
        Predicate<Nota> byGrupa = x -> x.getStudent().getGroup() == 225;
        Predicate<Nota> byProf = x -> x.getProfesor().equals("Antonin");
        Predicate<Nota> filter1 = byGrupa.and(byProf);

        l.stream().filter(filter1).forEach(System.out::println);
    }

    private static void report2(List<Nota> l) {
        Map<Student, List<Nota>> map = l.stream()
                .collect(Collectors.groupingBy(x -> x.getStudent()));

        map.entrySet().forEach(x -> {
            System.out.println(x.getKey().getName());
            int count = x.getValue().size();
            double sum = x.getValue().stream()
                    .map(y -> y.getValue()).reduce(0d, (a,b) -> a + b);
            System.out.println(sum / count);
        });


    }
}
