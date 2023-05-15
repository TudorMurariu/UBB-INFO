package domain.validators;

import domain.OrderItem;

public class OrderItemValidator implements Validator<OrderItem> {
    @Override
    public void validate(OrderItem entity) throws ValidationException {
        String error = "";
        if(entity.getDrugName().equals(""))
            error += "The name of drug cannot be empty !\n";
        if(entity.getQuantity() <= 0)
            error += "The quantity of the order needs to be positive !\n";
        if(!error.equals(""))
            throw new ValidationException(error);
    }
}
