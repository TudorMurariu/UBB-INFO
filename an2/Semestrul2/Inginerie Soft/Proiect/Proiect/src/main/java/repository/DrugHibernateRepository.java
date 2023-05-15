package repository;

import domain.Drug;
import domain.validators.Validator;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;

import java.util.ArrayList;
import java.util.Collection;

public class DrugHibernateRepository implements IDrugRepository{

    private SessionFactory sessionFactory;
    private Validator<Drug> drugValidator;

    public DrugHibernateRepository(SessionFactory sessionFactory, Validator<Drug> drugValidator) {
        this.sessionFactory = sessionFactory;
        this.drugValidator = drugValidator;
    }

    @Override
    public void save(Drug elem) {
        drugValidator.validate(elem);
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                session.save(elem);
                tx.commit();
            }
            catch (RuntimeException ex){
                System.err.println("Error occurred to addDrug method: " + ex.getMessage());
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
                String selectStmt = "from Drug where id = :idDrug";
                Drug deletedDrug = session.createQuery(selectStmt, Drug.class)
                        .setParameter("idDrug", ID)
                        .setMaxResults(1)
                        .uniqueResult();
                session.delete(deletedDrug);
                tx.commit();
            }catch (RuntimeException ex){
                System.err.println("Error occurred to deleteDrug method: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
            }
        }
    }

    @Override
    public void update(Drug elem) {
        drugValidator.validate(elem);
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                System.out.println("ID: " + elem.getID());
                Drug updateDrug = session.load(Drug.class, elem.getID());
                updateDrug.setDescription(elem.getDescription());
                updateDrug.setName(elem.getName());
                updateDrug.setPrice(elem.getPrice());
                session.update(updateDrug);
                tx.commit();
            }catch (RuntimeException ex){
                System.err.println("Error occurred to updateDrug method: " + ex.getMessage());
                if(tx != null)
                    tx.rollback();
            }
        }
    }

    @Override
    public Drug find(Integer ID) {
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                String selectStmt = "from Drug where id = :idDrug";
                Drug findDrug = session.createQuery(selectStmt, Drug.class)
                        .setParameter("idDrug", ID)
                        .setMaxResults(1)
                        .uniqueResult();
                tx.commit();
                return findDrug;
            }catch (RuntimeException ex){
                System.err.println("Error occurred to find Drug " + ex.getMessage());
                if(tx == null)
                    tx.rollback();
                return null;
            }
        }
    }

    @Override
    public Collection<Drug> findAll() {
        Collection<Drug> drugs = new ArrayList<>();
        try(Session session = sessionFactory.openSession()){
            Transaction tx = null;
            try{
                tx = session.beginTransaction();
                String selectStmt = "from Drug";
                drugs = session.createQuery(selectStmt, Drug.class).list();
                tx.commit();
                return drugs;
            }catch (RuntimeException ex){
                System.err.println("Error occurred to findAll Drug " + ex.getMessage());
                if(tx == null)
                    tx.rollback();
                return new ArrayList<>();
            }
        }
    }
}
