import domain.Nota;
import domain.NotaDto;
import domain.Student;
import domain.Tema;

import java.time.LocalDate;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class TestSem6 {
    private static List<Student> getStudents() {
        Student s1 = new Student("andrei", 221);
        s1.setId(2l);
        Student s2 = new Student("dan", 222);
        s2.setId(2l);
        Student s3 = new Student("gigi", 221);
        s3.setId(2l);
        Student s4 = new Student("costel", 222);
        s4.setId(2l);
        return Arrays.asList(s1, s2, s3, s4);
    }

    private static List<Tema> getTeme() {
        return Arrays.asList(
                new Tema("t1", "desc1"),
                new Tema("t2", "desc2"),
                new Tema("t3", "desc3"),
                new Tema("t4", "desc4")
        );
    }

    private static List<Nota> getNote(List<Student> stud, List<Tema> teme) {
        return Arrays.asList(
                new Nota(stud.get(0), teme.get(0), 10d, LocalDate.of(2019, 11, 2), "profesor1"),
                new Nota(stud.get(1), teme.get(0), 9d, LocalDate.of(2019, 11, 2).minusWeeks(1), "profesor1"),
                new Nota(stud.get(1), teme.get(1), 10d, LocalDate.of(2019, 10, 20), "profesor2"),
                new Nota(stud.get(1), teme.get(2), 10d, LocalDate.of(2019, 10, 20), "profesor2"),
                new Nota(stud.get(2), teme.get(1), 6d, LocalDate.of(2019, 10, 28), "profesor1"),
                new Nota(stud.get(2), teme.get(3), 9d, LocalDate.of(2019, 10, 27), "profesor2"),
                new Nota(stud.get(1), teme.get(3), 10d, LocalDate.of(2019, 10, 29), "profesor2")
        );
    }

    public static void main(String[] args) {
//        NotaDto.forEach(x-> System.out.println(x));
//        System.out.println(LocalDate.of(2019,10,29).format(DateTimeFormatter.ofPattern("dd/MM/yyyy")));
//
        List<Nota> note = getNote(getStudents(), getTeme());

//        report1(note);
//        report2(note);
       // report4(note);
        afisare4(note);

//        note.stream()
//                .map(n -> n.getStudent().getName()+ " "+n.getValue()+ " "+n.getTema().getId())
//                .forEach(System.out::println);

    }

    /**
     * tema cea mai grea: media notelor cea mai mica
     *
     * @param note
     */
    private static void report4(List<Nota> note) {
        Map<Tema, List<Nota>> grouped=note.stream()
                .collect(Collectors.groupingBy(x->x.getTema()));
        Optional<Double> res=grouped.entrySet()
                .stream()
                .map(x->{
                    Double suma=x.getValue()
                            .stream()
                            .map(y->y.getValue())
                            .reduce(0d, (a,b)->a+b);
                    return suma/4d;
                })
                .min((x,y)->x.compareTo(y));


        res.ifPresent(System.out::println);

    }

        public static void afisare4(List<Nota> note){
        Map<Tema, List<Nota>> temaListMap = note.stream()
                .collect(Collectors.groupingBy(x -> x.getTema()));

         Map.Entry<Tema, Double> res=temaListMap.entrySet()
                    .stream()
                .map(x -> {
                    double suma = x.getValue().stream()
                            .map(y -> y.getValue())
                            .reduce(0d, (a, b) -> a + b);

                    double media = suma/x.getValue().size();
                    Map.Entry<Tema, Double> res2=new AbstractMap.SimpleEntry<Tema, Double>(x.getKey(), media);
                    return res2;
                })
                 .min(Comparator.comparing(Map.Entry::getValue)).get();
            System.out.println(res.getKey().getDesc()+", "+ res.getValue());

    }

    /**
     * media notelor la o anumita tema
     *
     * @param note
     */
    private static void report3(List<Nota> note) {
        Map<Tema, List<Nota>> grouped=note.stream()
                .collect(Collectors.groupingBy(x->x.getTema()));
        List<Nota> n=note.stream()
                .filter(x->x.getTema().getId().equals("t1"))
                .collect(Collectors.toList());
        double avrg=n.stream()
                .map(x->x.getValue())
                .reduce(0d,(x,y)->x+y)/(n.size());
        System.out.println(avrg);

//        grouped.entrySet()
//                .forEach(x->{
//                    Double suma=x.getValue()
//                            .stream()
//                            .map(y->y.getValue())
//                            .reduce(0d, (a,b)->a+b);
//                    System.out.println(suma/4);
//
//                });
    }

    /**
     * creati/afisati o lista de obiecte NotaDto apoi filtrati dupa un anumit profesor
     * (toate notele acordate de un anumit profesor)
     * (toate notele acordate de un anumit profesor, la o anumita grupa)
     * GENERALIZARE: FILTRU COMPUS
     */
    private static void report1(List<Nota> note) {
        Predicate<Nota> byGrupa=x->x.getStudent().getGroup()==221;
        Predicate<Nota> byProf=x->x.getProfesor().equals("profesor1");
        Predicate<Nota> filtered=byGrupa.and(byProf);

        note.stream()
                .filter(x->x.getStudent().getGroup()==221)
                .map(x->new NotaDto(x.getStudent().getName(),x.getTema().getId(),x.getValue(),x.getProfesor()))
                .forEach(x->System.out.println(x));
    }

    /**
     * media notelor de lab pt fiecare student
     *
     * @param note
     */
    private static void report2(List<Nota> note) {

        Map<Student, List<Nota>> grouped=note.stream()
                .collect(Collectors.groupingBy(x->x.getStudent()));

        grouped.entrySet()
                .forEach(x->{
                    System.out.print(x.getKey().getName());
                    Double suma=x.getValue()
                            .stream()
                            .map(y->y.getValue())
                            .reduce(0d, (a,b)->a+b);
                    System.out.println(suma/4);

                });
    }





}
