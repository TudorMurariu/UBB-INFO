package repository;

import domain.OrderItem;
import javafx.util.Pair;

public interface IOrderItemRepository extends Repository<OrderItem, Pair<Integer,Integer>> {
}
