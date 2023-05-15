package repository;

import domain.TypeUser;
import domain.User;
import domain.validators.ValidationException;
import domain.validators.Validator;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;

public class UserDBRepository implements IUserRepository {

    private JdbcUtils jdbcUtils;
    private Validator<User> validator;

    public UserDBRepository(Properties properties, Validator<User> validator) {
        this.validator = validator;
        jdbcUtils = new JdbcUtils(properties);
    }

    @Override
    public void save(User elem) {
        validator.validate(elem);
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement insertStmt = connection
                    .prepareStatement("insert into users(username, password, type) values (?,?,?)")) {
            insertStmt.setString(1, elem.getUsername());
            insertStmt.setString(2, elem.getPassword());
            insertStmt.setString(3, elem.getType().toString());
            insertStmt.executeUpdate();
        } catch (SQLException e) {
            throw new ValidationException("The username is already used!");
        }
    }

    @Override
    public void delete(Integer ID) {
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement deleteStmt = connection
                    .prepareStatement("delete from users where id = ?")) {
            deleteStmt.setInt(1, ID);
            deleteStmt.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void update(User elem) {
        validator.validate(elem);
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement updateStmt = connection
                    .prepareStatement("update users set username=?, password=?, type=?" +
                            "where id=?")) {
            updateStmt.setString(1, elem.getUsername());
            updateStmt.setString(2, elem.getPassword());
            updateStmt.setString(3, elem.getType().toString());
            updateStmt.setInt(4, elem.getID());
            updateStmt.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public User find(Integer ID) {
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement findStmt = connection
                    .prepareStatement("select * from users where id=?")) {
            findStmt.setInt(1, ID);
            ResultSet resultSet = findStmt.executeQuery();
            if(resultSet.next() == false)
                return null;
            Integer id = resultSet.getInt("id");
            String username = resultSet.getString("username");
            String password = resultSet.getString("password");
            TypeUser type = TypeUser.valueOf(resultSet.getString("type"));
            User user = new User(id, username, password, type);
            return user;
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public Collection<User> findAll() {
        Collection<User> users = new ArrayList<>();
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement stmt = connection.prepareStatement("select * from users")) {
            ResultSet resultSet = stmt.executeQuery();
            while (resultSet.next()){
                Integer id = resultSet.getInt("id");
                String username = resultSet.getString("username");
                String password = resultSet.getString("password");
                TypeUser type = TypeUser.valueOf(resultSet.getString("type"));
                User user = new User(id, username, password, type);
                users.add(user);
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return users;
    }

    @Override
    public User filterByUsernameAndPassword(User user) {
        validator.validate(user);
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement filterStmt = connection
                    .prepareStatement("select * from users where username = ? and password = ? and " +
                            "type = ?")) {
            filterStmt.setString(1, user.getUsername());
            filterStmt.setString(2, user.getPassword());
            filterStmt.setString(3, user.getType().toString());
            ResultSet resultSet = filterStmt.executeQuery();
            if(resultSet.next() == false)
                return null;
            Integer id = resultSet.getInt("id");
            user.setID(id);
            return user;
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return null;
    }
}
