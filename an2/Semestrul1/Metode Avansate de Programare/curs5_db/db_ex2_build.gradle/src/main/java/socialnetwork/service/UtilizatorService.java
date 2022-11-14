package socialnetwork.service;

import socialnetwork.domain.Utilizator;
import socialnetwork.repository.Repository;



import java.util.List;
import java.util.Optional;

import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public class UtilizatorService {
    private Repository<Long, Utilizator> repo;

    public UtilizatorService(Repository<Long, Utilizator> repo) {
        this.repo = repo;
    }

    public Optional<Utilizator> addUtilizator(Utilizator utilizator) {
        return repo.save(utilizator);
    }

    public List<Utilizator> getAllUsers() {
        Iterable<Utilizator> students = repo.findAll();
        return StreamSupport.stream(students.spliterator(), false).collect(Collectors.toList());
    }


    public List<Utilizator> filterUsersName(String s) {
        Iterable<Utilizator> students = repo.findAll();

        List<Utilizator> filteredUsers = StreamSupport.stream(students.spliterator(), false)
                .filter(user -> user.getFirstName().contains(s)).collect(Collectors.toList());


//        Set<Utilizator> filteredUsers1= new HashSet<>();
//        students.forEach(filteredUsers1::add);
//        filteredUsers1.removeIf(student -> !student.getFirstName().contains(s));

        return filteredUsers;
    }

    public Iterable<Utilizator> getAll(){
        return repo.findAll();
    }




    ///TO DO: add other methods
}
