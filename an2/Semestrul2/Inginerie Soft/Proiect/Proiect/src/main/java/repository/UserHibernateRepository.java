package repository;

import domain.Drug;
import domain.User;
import domain.validators.Validator;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;

import java.util.ArrayList;
import java.util.Collection;

public class UserHibernateRepository implements IUserRepository{

    private SessionFactory sessionFactory;
    private Validator<User> userValidator;

    public UserHibernateRepository(SessionFactory sessionFactory,Validator<User> userValidator) {
        this.sessionFactory = sessionFactory;
        this.userValidator = userValidator;
    }

    @Override
    public User filterByUsernameAndPassword(User user) {
        userValidator.validate(user);
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                String selectStmt = "from User where username = :username and password = :password and type = :type";
                User findUSer = session.createQuery(selectStmt, User.class)
                        .setParameter("username", user.getUsername())
                        .setParameter("password", user.getPassword())
                        .setParameter("type", user.getType())
                        .setMaxResults(1)
                        .uniqueResult();
                tx.commit();
                return findUSer;
            }catch (RuntimeException ex){
                System.err.println("Error occurred to filterByUsernameAndPassword User: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
                return null;
            }
        }
    }

    @Override
    public void save(User elem) {
        userValidator.validate(elem);
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                session.save(elem);
                tx.commit();
            }catch (RuntimeException ex){
                System.err.println("Error occurred to save User: " + ex.getMessage());
                if(tx == null)
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
                String selectStmt = "from User where id = :idUser";
                User findUser = session.createQuery(selectStmt, User.class)
                        .setParameter("idUser", ID)
                        .setMaxResults(1)
                        .uniqueResult();
                session.delete(findUser);
                tx.commit();
            }catch (RuntimeException ex){
                System.err.println("Error occurred to delete User: " + ex.getMessage());
                if(tx == null)
                    tx.rollback();
            }
        }
    }

    @Override
    public void update(User elem) {
        userValidator.validate(elem);
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                User updateUser = session.load(User.class, elem.getID());
                updateUser.setUsername(elem.getUsername());
                updateUser.setPassword(elem.getPassword());
                session.update(updateUser);
                tx.commit();
            }catch (RuntimeException ex){
                System.err.println("Error occurred to update User: " + ex.getMessage());
                if(tx == null)
                    tx.rollback();
            }
        }
    }

    @Override
    public User find(Integer ID) {
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                String selectStmt = "from User where id = :idUser";
                User findUser = session.createQuery(selectStmt, User.class)
                        .setParameter("idUser", ID)
                        .setMaxResults(1)
                        .uniqueResult();
                tx.commit();
                return findUser;
            }catch (RuntimeException ex){
                System.err.println("Error occurred to find User " + ex.getMessage());
                if(tx == null)
                    tx.rollback();
                return null;
            }
        }
    }

    @Override
    public Collection<User> findAll() {
        Collection<User> users = new ArrayList<>();
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                String selectStmt = "from User";
                users = session.createQuery(selectStmt, User.class).list();
                tx.commit();
                return users;
            }catch (RuntimeException ex){
                System.err.println("Error occurred to findAll User " + ex.getMessage());
                if(tx == null)
                    tx.rollback();
                return new ArrayList<>();
            }
        }
    }
}
