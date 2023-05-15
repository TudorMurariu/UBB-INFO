package domain;



import javafx.util.Pair;

import java.io.Serializable;
import java.util.Objects;

public class OrderItem extends Entity<Pair<Integer,Integer>> implements Serializable {

    private String drugName;
    private Integer quantity;
    private Drug drug;
    private Order order;

    public OrderItem() {}

    public OrderItem(Pair<Integer,Integer> ID,String drugName, Integer quantity, Drug drug, Order order) {
        this.ID = ID;
        this.drugName = drugName;
        this.quantity = quantity;
        this.drug = drug;
        this.order = order;
    }

    public OrderItem(String drugName, Integer quantity, Drug drug, Order order) {
        this.drugName = drugName;
        this.quantity = quantity;
        this.drug = drug;
        this.order = order;
    }

    public String getDrugName() {
        return drugName;
    }

    public void setDrugName(String drugName) {
        this.drugName = drugName;
    }

    public Integer getQuantity() {
        return quantity;
    }

    public void setQuantity(Integer quantity) {
        this.quantity = quantity;
    }

    public Drug getDrug() {
        return drug;
    }

    public void setDrug(Drug drug) {
        this.drug = drug;
    }

    public Order getOrder() {
        return order;
    }

    public void setOrder(Order order) {
        this.order = order;
    }

    @Override
    public String toString() {
        return "OrderItem{" +
                "ID=" + ID +
                ", drugName='" + drugName + '\'' +
                ", quantity=" + quantity +
                ", drug=" + drug +
                ", order=" + order +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        OrderItem orderItem = (OrderItem) o;
        return Objects.equals(drugName, orderItem.drugName) && Objects.equals(quantity, orderItem.quantity) && Objects.equals(drug, orderItem.drug) && Objects.equals(order, orderItem.order);
    }

    @Override
    public int hashCode() {
        return Objects.hash(drugName, quantity, drug, order);
    }
}
