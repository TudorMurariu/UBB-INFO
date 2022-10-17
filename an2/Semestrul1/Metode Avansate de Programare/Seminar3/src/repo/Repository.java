package repo;

import models.ValidationException;

public interface Repository<E, ID> {
    E save(E entity) throws ValidationException;
    E delete(ID id);
    E findOne(ID id);
    Iterable<E> findAll();
}
