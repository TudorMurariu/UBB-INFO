package domain;

public class Tema {

    private String id;

    private String desc;

    public Tema(String id, String desc) {
        this.id = id;
        this.desc=desc;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getDesc() {
        return desc;
    }

    public void setDesc(String desc) {
        this.desc = desc;
    }

    @Override
    public String toString() {
        return "Tema{" +
                "id='" + id + '\'' +
                ", desc='" + desc + '\'' +
                '}';
    }
}

