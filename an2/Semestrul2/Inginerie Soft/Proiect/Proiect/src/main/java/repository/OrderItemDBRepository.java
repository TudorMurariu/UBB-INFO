package repository;

import domain.Order;
import domain.OrderItem;
import domain.validators.Validator;
import javafx.util.Pair;

import java.util.Collection;
import java.util.Properties;

public class OrderItemDBRepository implements IOrderItemRepository{

    private JdbcUtils jdbcUtils;
    private Validator<OrderItem> validator;

    public OrderItemDBRepository(Properties properties, Validator<OrderItem> validator) {
        this.validator = validator;
        jdbcUtils = new JdbcUtils(properties);
    }

    @Override
    public void save(OrderItem elem) {

    }

    @Override
    public void delete(Pair<Integer,Integer> ID) {

    }

    @Override
    public void update(OrderItem elem) {

    }

    @Override
    public OrderItem find(Pair<Integer,Integer> ID) {
        return null;
    }

    @Override
    public Collection<OrderItem> findAll() {
        return null;
    }
}
