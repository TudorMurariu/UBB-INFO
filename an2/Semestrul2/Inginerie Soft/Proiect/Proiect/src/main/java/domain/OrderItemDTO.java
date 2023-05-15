package domain;

import java.util.Objects;

public class OrderItemDTO {

    private String name;
    private Integer quantity;
    private Status status;

    public OrderItemDTO(String name, Integer quantity, Status status) {
        this.name = name;
        this.quantity = quantity;
        this.status = status;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getQuantity() {
        return quantity;
    }

    public void setQuantity(Integer quantity) {
        this.quantity = quantity;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        OrderItemDTO that = (OrderItemDTO) o;
        return Objects.equals(name, that.name) && Objects.equals(quantity, that.quantity) && status == that.status;
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, quantity, status);
    }

    @Override
    public String toString() {
        return "OrderItemDTO{" +
                "name='" + name + '\'' +
                ", quantity=" + quantity +
                ", status=" + status +
                '}';
    }
}
