package ro.ubbcluj.map.domain.validators;

import  ro.ubbcluj.map.domain.validators.ValidationException;

public interface Validator<T> {
    void validate(T entity) throws ValidationException;
}