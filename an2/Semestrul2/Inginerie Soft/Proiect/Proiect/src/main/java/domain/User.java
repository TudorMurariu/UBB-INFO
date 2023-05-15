package domain;

import java.io.Serial;
import java.io.Serializable;
import java.util.Objects;

public class User extends Entity<Integer> implements Serializable {

    private String username;
    private String password;
    private TypeUser type;

    public User() {}

    public User(Integer ID,String username, String password, TypeUser type) {
        this.ID = ID;
        this.username = username;
        this.password = password;
        this.type = type;
    }

    public User(String username, String password, TypeUser type) {
        this.username = username;
        this.password = password;
        this.type = type;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public TypeUser getType() {
        return type;
    }

    public void setType(TypeUser type) {
        this.type = type;
    }

    @Override
    public String toString() {
        return "User{" +
                "ID=" + ID +
                ", username='" + username + '\'' +
                ", password='" + password + '\'' +
                ", type='" + type + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        User user = (User) o;
        return Objects.equals(username, user.username) && Objects.equals(password, user.password) && Objects.equals(type, user.type);
    }

    @Override
    public int hashCode() {
        return Objects.hash(username, password, type);
    }
}
