package domain;

import java.io.Serializable;
import java.util.Objects;

public class Order extends Entity<Integer> implements Serializable {

    private Integer quantity;
    private User user;
    private Status status;

    public Order() {}

    public Order(Integer ID,Integer quantity, User user, Status status) {
        this.ID = ID;
        this.quantity = quantity;
        this.user = user;
        this.status = status;
    }

    public Order(Integer quantity, User user, Status status) {
        this.quantity = quantity;
        this.user = user;
        this.status = status;
    }

    public Integer getQuantity() {
        return quantity;
    }

    public void setQuantity(Integer quantity) {
        this.quantity = quantity;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }

    @Override
    public String toString() {
        return "Order{" +
                "id " + ID + '\'' +
                "quantity=" + quantity +
                ", idSection=" + user +
                ", status=" + status +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Order order = (Order) o;
        return Objects.equals(quantity, order.quantity) && Objects.equals(user, order.user) && status == order.status;
    }

    @Override
    public int hashCode() {
        return Objects.hash(quantity, user, status);
    }
}
