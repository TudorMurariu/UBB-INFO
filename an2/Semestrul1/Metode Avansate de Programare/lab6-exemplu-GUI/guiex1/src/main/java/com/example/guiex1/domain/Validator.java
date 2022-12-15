package com.example.guiex1.domain;

public interface Validator<T> {
    void validate(T entity) throws ValidationException;
}