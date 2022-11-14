package socialnetwork.service;

import socialnetwork.domain.Utilizator;
import socialnetwork.repository.Repository0;

public class UtilizatorService0 {
    private Repository0<Long, Utilizator> repo;

    public UtilizatorService0(Repository0<Long, Utilizator> repo) {
        this.repo = repo;
    }

    public Utilizator addUtilizator(Utilizator messageTask) {
        Utilizator task = repo.save(messageTask);
        return task;
    }

    public Iterable<Utilizator> getAll(){
        return repo.findAll();
    }

    ///TO DO: add other methods
}
