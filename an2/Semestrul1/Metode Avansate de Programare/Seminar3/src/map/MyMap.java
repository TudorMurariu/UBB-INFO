package map;

import models.Student;

import java.util.*;

public class MyMap {
    private Map<Integer, List<Student>> map;

    public MyMap() {
        map = new TreeMap<>(new StudentComparator());
    }

    public static class StudentComparator implements Comparator<Integer> {
        @Override
        public int compare(Integer o1, Integer o2) {
            return o1-o2;
        }
    }

    public void add(Student s) {
        Integer media_rot = s.getMediaRotunjita();
        List<Student> lista = map.get(media_rot);
        if(lista == null)
            lista = new ArrayList<>();
        lista.add(s);
        map.put(media_rot, lista);
    }

    public void afisare() {
        for(Map.Entry<Integer, List<Student>> x : map.entrySet())
            System.out.println(x.getKey() + " " + x.getValue());
    }

    public Set<Map.Entry<Integer, List<Student>>> getEntries() {
        return map.entrySet();
    }

}
