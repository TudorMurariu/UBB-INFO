package repo;

import models.ValidationException;
import models.Validator;

import java.util.HashMap;
import java.util.Map;

public abstract class InMemoryRepository<E extends HasID<ID>,   ID> implements Repository<E, ID> {

    private Map<ID, E> entities;
    private Validator<E> validator;

    public InMemoryRepository(Validator<E> validator) {
        entities = new HashMap<>();
        this.validator = validator;
    }

    @Override
    public E save(E entity) throws ValidationException {
        if(entity == null)
            throw new IllegalArgumentException("Entity cannot be null");
        else if(entities.containsKey(entity.getId()))
            return entities.get(entity.getId());

        validator.validate(entity);
        entities.put(entity.getId(), entity);
        return  entity;
    }

    @Override
    public E delete(ID id) {
        return null;
    }

    @Override
    public E findOne(ID id) {
        return null;
    }

    @Override
    public Iterable<E> findAll() {
        return null;
    }
}
