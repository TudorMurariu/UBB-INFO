package repository;

import domain.Order;
import domain.OrderItem;
import domain.validators.Validator;
import javafx.util.Pair;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;

import java.util.ArrayList;
import java.util.Collection;

public class OrderItemHibernateRepository implements IOrderItemRepository{

    private SessionFactory sessionFactory;
    private Validator<OrderItem> orderItemValidator;

    public OrderItemHibernateRepository(SessionFactory sessionFactory, Validator<OrderItem> orderItemValidator) {
        this.sessionFactory = sessionFactory;
        this.orderItemValidator = orderItemValidator;
    }

    @Override
    public void save(OrderItem elem) {
        orderItemValidator.validate(elem);
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                session.save(elem);
                tx.commit();
            }catch (RuntimeException ex){
                System.err.println("Error occurred to save method OrderItems: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
            }
        }
    }

    @Override
    public void delete(Pair<Integer,Integer> ID) {
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                String selectStmt = "from OrderItem where id = :idOrderItem";
                OrderItem orderItem = session.createQuery(selectStmt, OrderItem.class)
                                .setParameter("idOrderItem", ID)
                                .setMaxResults(1)
                                .uniqueResult();
                session.delete(orderItem);
                tx.commit();
            }catch (RuntimeException ex){
                System.err.println("Error occurred to delete method OrderItems: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
            }
        }
    }

    @Override
    public void update(OrderItem elem) {
        orderItemValidator.validate(elem);
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                OrderItem orderItem = session.load(OrderItem.class, elem.getID());
                orderItem.setOrder(elem.getOrder());
                orderItem.setDrug(elem.getDrug());
                orderItem.setDrugName(elem.getDrugName());
                orderItem.setQuantity(elem.getQuantity());
                orderItem.setID(new Pair<Integer,Integer>(elem.getOrder().getID(), elem.getDrug().getID()));
                tx.commit();
            }catch (RuntimeException ex){
                System.err.println("Error occurred to update method OrderItems: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
            }
        }
    }

    @Override
    public OrderItem find(Pair<Integer,Integer> ID) {
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                String selectStmt = "from OrderItem where id = :idOrderItem";
                OrderItem orderItem = session.createQuery(selectStmt, OrderItem.class)
                        .setParameter("idOrderItem", ID)
                        .setMaxResults(1)
                        .uniqueResult();
                tx.commit();
                return orderItem;
            }catch (RuntimeException ex){
                System.err.println("Error occurred to find method OrderItems: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
                return null;
            }
        }
    }

    @Override
    public Collection<OrderItem> findAll() {
        Collection<OrderItem> orderItems = new ArrayList<>();
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                String selectStmt = "from OrderItem";
                orderItems = session.createQuery(selectStmt, OrderItem.class).list();
                tx.commit();
                return orderItems;
            }catch (RuntimeException ex){
                System.err.println("Error occurred to findAll method OrderItems: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
                return new ArrayList<>();
            }
        }
    }
}
