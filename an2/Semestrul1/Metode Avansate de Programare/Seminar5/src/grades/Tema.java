package grades;

public class Tema {

    private String desc;

    private String id;

    public Tema(String desc, String id) {
        this.desc = desc;
        this.id = id;
    }

    public String getDesc() {
        return desc;
    }

    public void setDesc(String desc) {
        this.desc = desc;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    @Override
    public String toString() {
        return "Tema{" +
                "desc='" + desc + '\'' +
                ", id='" + id + '\'' +
                '}';
    }
}
