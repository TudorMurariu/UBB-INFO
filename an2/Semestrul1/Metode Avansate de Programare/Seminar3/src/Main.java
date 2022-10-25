import map.MyMap;
import models.Student;

import java.lang.reflect.Array;
import java.util.*;


public class Main {
    public static void main(String[] args) {
        Student s1 = new    Student("Dan", 4.5f);
        Student  s2 = new    Student("Ana", 8.5f);
        Student  s3 = new    Student("Alexandru", 5.5f);
        Student  s4 = new    Student("Beatrice", 4.6f);
        Student  s5 = new    Student("Daniel", 5.3f);
        Student  s6 = new    Student("Matei", 9.3f);
        //System.out.println(s1);

        HashSet<Student> set = new HashSet<>();
        set.add(s1);
        set.add(s2);
        set.add(s3);

        //for(var x : set.toArray())
            //System.out.println(x);

        System.out.println("hashSet:\n" + set);

        TreeSet<Student> treeSet = new TreeSet<>(new Comparator<Student>() {
            @Override
            public int compare(Student o1, Student o2) {
                return o1.getNume().compareTo(o2.getNume());
            }
        });
        treeSet.addAll(Arrays.asList(s1,s2,s3));

        System.out.println("treeSet:\n" + treeSet);


        Map<String, Student> hashmap = new HashMap<>();
        hashmap.put(s1.getNume(), s1);
        hashmap.put(s2.getNume(), s2);
        hashmap.put(s3.getNume(), s3);

        System.out.println("\nhashMap:");
        for(Map.Entry<String, Student> x : hashmap.entrySet())
            System.out.println(x.getKey() + " " + x.getValue());

        Map<String, Student> treemap = new TreeMap<>();
        treemap.put(s1.getNume(), s1);
        treemap.put(s2.getNume(), s2);
        treemap.put(s3.getNume(), s3);

        System.out.println("\ntreemap:");
        for(Map.Entry<String, Student> x : treemap.entrySet())
            System.out.println(x.getKey() + " " + x.getValue());

        System.out.println("\nMyMap:");
        MyMap map = new MyMap();
        map.add(s1);
        map.add(s2);
        map.add(s3);
        map.add(s4);
        map.add(s5);
        map.add(s6);
        map.afisare();
    }
}