package grades;

public class Entity<ID> {
    private ID id;

    public void setId(ID id) {
        this.id = id;
    }

    public ID getId() {
        return id;
    }
}
