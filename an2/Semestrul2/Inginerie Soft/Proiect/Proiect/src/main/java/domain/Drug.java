package domain;

import java.io.Serializable;
import java.util.Objects;

public class Drug extends Entity<Integer> {

    private String name;
    private Float price;
    private String description;

    public Drug(){}

    public Drug(Integer ID,String name, Float price, String description) {
        this.ID = ID;
        this.name = name;
        this.price = price;
        this.description = description;
    }

    public Drug(String name, Float price, String description) {
        this.name = name;
        this.price = price;
        this.description = description;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Float getPrice() {
        return price;
    }

    public void setPrice(Float price) {
        this.price = price;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }


    @Override
    public String toString() {
        return "Drug{" +
                "id " + ID + '\'' +
                "name='" + name + '\'' +
                ", price=" + price +
                ", description='" + description + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Drug drug = (Drug) o;
        return Objects.equals(name, drug.name) && Objects.equals(price, drug.price) && Objects.equals(description, drug.description);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, price, description);
    }
}
