package domain;

import java.io.Serializable;

public class Entity<ID> implements Serializable {

    public Entity(){}

    protected ID ID;

    public ID getID() {
        return ID;
    }

    public void setID(ID ID) {
        this.ID = ID;
    }
}
