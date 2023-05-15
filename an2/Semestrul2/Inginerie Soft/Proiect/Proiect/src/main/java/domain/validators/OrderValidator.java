package domain.validators;

import domain.Order;

public class OrderValidator implements Validator<Order> {
    @Override
    public void validate(Order entity) throws ValidationException {
        String error = "";
        if(entity.getQuantity() <= 0)
            error += "The quantity of the order needs to be positive !\n";
        if(!error.equals(""))
            throw new ValidationException(error);
    }
}
