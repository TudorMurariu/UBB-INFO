package utils.events;

import domain.Order;

public class OrderEvent implements Event{
    private OrderEventType type;
    private Order data, oldData;

    public OrderEvent(OrderEventType type, Order data){
        this.type = type;
        this.data = data;
    }

    public OrderEvent(OrderEventType type, Order data, Order oldData){
        this.type = type;
        this.data = data;
        this.oldData = oldData;
    }

    public OrderEventType getType() {
        return type;
    }

    public Order getData() {
        return data;
    }

    public Order getOldData() {
        return oldData;
    }
}
