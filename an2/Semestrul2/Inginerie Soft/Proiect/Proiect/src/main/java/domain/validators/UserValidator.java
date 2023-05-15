package domain.validators;

import domain.User;

public class UserValidator implements Validator<User> {
    @Override
    public void validate(User entity) throws ValidationException {
        String error = "";
        if(entity.getUsername().equals(""))
            error += "The username cannot be empty !\n";
        if(entity.getPassword().equals(""))
            error += "The password cannot be empty !\n";
        if(entity.getType() == null)
            error += "Invalid type of User !\n";
        if(!error.equals(""))
            throw new ValidationException(error);
    }
}
