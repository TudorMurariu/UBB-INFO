package socialnetwork;

import socialnetwork.domain.Utilizator;
import socialnetwork.domain.UtilizatorValidator;
import socialnetwork.repository.Repository;
import socialnetwork.repository.dbrepo.UtilizatorDbRepository;

public class Main {
    public static void main(String[] args) {

        System.out.println("Hello world!");
        System.out.println("Reading data from file");
        String username="postgres";
        String pasword="postgres";
        String url="jdbc:postgresql://localhost:5434/socialnetwork";
        Repository<Long, Utilizator> userFileRepository3 =
                new UtilizatorDbRepository(url,username, pasword, new UtilizatorValidator());

        userFileRepository3.findAll().forEach(x-> System.out.println(x));
    }
}