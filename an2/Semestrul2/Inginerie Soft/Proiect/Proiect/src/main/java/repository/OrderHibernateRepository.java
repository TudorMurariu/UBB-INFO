package repository;

import domain.Order;
import domain.validators.Validator;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;

import java.util.ArrayList;
import java.util.Collection;

public class OrderHibernateRepository implements IOrderRepository{

    private SessionFactory sessionFactory;
    private Validator<Order> orderValidator;

    public OrderHibernateRepository(SessionFactory sessionFactory, Validator<Order> orderValidator) {
        this.sessionFactory = sessionFactory;
        this.orderValidator = orderValidator;
    }

    @Override
    public void save(Order elem) {
        orderValidator.validate(elem);
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                System.out.println(tx);
                System.out.println("AAAAAAAAAAAAAAAAAAAAAAAAAA");
                session.save(elem);
                System.out.println("BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB");
                tx.commit();
                System.out.println("CCCCCCCCCCCCCccc");
                Collection<Order> orders = findAll();
                Integer maxiID = 0;
                for(Order order: orders)
                    if(order.getID() >= maxiID)
                        maxiID = order.getID();
                elem.setID(maxiID);
            }catch (RuntimeException ex){
                System.err.println("Error occurred to save order: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
            }
        }
    }

    @Override
    public void delete(Integer ID) {
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                String selectStmt = "from Order where id = :idOrder";
                Order deletedOrder = session.createQuery(selectStmt, Order.class)
                        .setParameter("idOrder", ID)
                        .setMaxResults(1)
                        .uniqueResult();
                session.delete(deletedOrder);
                tx.commit();
            }catch (RuntimeException ex){
                System.err.println("Error occurred to delete order: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
            }
        }
    }

    @Override
    public void update(Order elem) {
        orderValidator.validate(elem);
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                Order updateOrder = session.load(Order.class, elem.getID());
                updateOrder.setUser(elem.getUser());
                updateOrder.setQuantity(elem.getQuantity());
                updateOrder.setStatus(elem.getStatus());
                session.update(updateOrder);
                tx.commit();
            }catch (RuntimeException ex){
                System.err.println("Error occurred to update order: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
            }
        }
    }

    @Override
    public Order find(Integer ID) {
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                String findStmt = "from Order where id = :idOrder";
                Order findOrder = session.createQuery(findStmt, Order.class)
                        .setParameter("idOrder", ID)
                        .setMaxResults(1)
                        .uniqueResult();
                tx.commit();
                return findOrder;
            }catch (RuntimeException ex){
                System.err.println("Error occurred to find order: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
                return null;
            }
        }
    }

    @Override
    public Collection<Order> findAll() {
        Collection<Order> orders = new ArrayList<>();
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                String findStmt = "from Order";
                orders = session.createQuery(findStmt, Order.class).list();
                tx.commit();
                return orders;
            }catch (RuntimeException ex){
                System.err.println("Error occurred to find order: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
                return new ArrayList<>();
            }
        }
    }
}
