package repository;

import domain.Drug;
import domain.Order;
import domain.Status;
import domain.validators.Validator;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;

public class OrderDBRepository implements IOrderRepository{

    private JdbcUtils jdbcUtils;
    private Validator<Order> validator;

    public OrderDBRepository(Properties properties, Validator<Order> validator) {
        this.validator = validator;
        jdbcUtils = new JdbcUtils(properties);
    }

    @Override
    public void save(Order elem) {
        validator.validate(elem);
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement insertStmt = connection
                    .prepareStatement("insert into orders(quantity, status, userid) values (?,?,?)")) {
            insertStmt.setInt(1, elem.getQuantity());
            insertStmt.setString(2, elem.getStatus().toString());
//            insertStmt.setInt(3, elem.getIdSection());
            insertStmt.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void delete(Integer ID) {
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement deleteStmt = connection
                    .prepareStatement("delete from orders where id=?")) {
            deleteStmt.setInt(1, ID);
            deleteStmt.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void update(Order elem) {
        validator.validate(elem);
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement updateStmt = connection
                    .prepareStatement("update from orders set quantity=?, status=?, userid=?" +
                            "where id=?")) {
            updateStmt.setInt(1, elem.getQuantity());
            updateStmt.setString(2, elem.getStatus().toString());
//            updateStmt.setInt(3, elem.getIdSection());
            updateStmt.setInt(4, elem.getID());
            updateStmt.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public Order find(Integer ID) {
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement findStmt = connection
                    .prepareStatement("select * from orders where id=?")) {
            findStmt.setInt(1, ID);
            ResultSet resultSet = findStmt.executeQuery();
            if(resultSet.next() == false)
                return  null;
            Integer id = resultSet.getInt("id");
            Integer quantity = resultSet.getInt("quantity");
            Status status = Status.valueOf( resultSet.getString("status") );
            Integer userid = resultSet.getInt("userid");
//            Order order = new Order(id, quantity, userid, status);
//            return order;
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public Collection<Order> findAll() {
        Collection<Order> orders = new ArrayList<>();
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement findStmt = connection
                    .prepareStatement("select * from orders ")) {
            ResultSet resultSet = findStmt.executeQuery();
            while (resultSet.next()) {
                Integer id = resultSet.getInt("id");
                Integer quantity = resultSet.getInt("quantity");
                Status status = Status.valueOf(resultSet.getString("status"));
                Integer userid = resultSet.getInt("userid");
//                Order order = new Order(id, quantity, userid, status);
//                orders.add(order);
            }

        } catch (SQLException e) {
            e.printStackTrace();
        }
        return orders;
    }
}
