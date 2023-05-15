package domain.validators;

public class ValidationException extends RuntimeException{
    public ValidationException(String msg) { super(msg); }
}
