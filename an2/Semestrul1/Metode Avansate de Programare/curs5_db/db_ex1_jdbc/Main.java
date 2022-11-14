package ro.ubbcluj.map;

import ro.ubbcluj.map.domain.Utilizator;
import ro.ubbcluj.map.domain.validators.UtilizatorValidator;
import ro.ubbcluj.map.repository.db.UtilizatorDbRepository;

public class Main {

    public static void main(String[] args) {
	// write your code here
        System.out.println("ok");
        UtilizatorDbRepository repoDB=new UtilizatorDbRepository("jdbc:postgresql://localhost:5433/academic","postgres","postgres", new UtilizatorValidator());
        Utilizator u=new Utilizator("aa","bb");
        repoDB.save(u);
        repoDB.findAll().forEach(System.out::println);
    }
}
