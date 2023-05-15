package domain.validators;

import domain.Drug;

public class DrugValidator implements Validator<Drug> {
    @Override
    public void validate(Drug entity) throws ValidationException {
        String error = "";
        if(entity.getName().equals(""))
            error += "The name of drug cannot be empty !\n";
        if(entity.getDescription().equals(""))
            error += "The description of drug cannot be empty !\n";
        if(entity.getPrice() <= 0.0)
            error += "The price of drug must be positive !\n";
        if(!error.equals(""))
            throw new ValidationException(error);
    }
}
