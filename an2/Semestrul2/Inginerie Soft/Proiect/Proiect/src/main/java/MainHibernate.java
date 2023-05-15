import domain.*;
import domain.validators.OrderItemValidator;
import domain.validators.OrderValidator;
import domain.validators.UserValidator;
import javafx.util.Pair;
import org.hibernate.SessionFactory;
import org.hibernate.boot.MetadataSources;
import org.hibernate.boot.registry.StandardServiceRegistry;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import repository.*;

public class MainHibernate {
    static SessionFactory sessionFactory;
    static void initialize() {
        // A SessionFactory is set up once for an application!
        final StandardServiceRegistry registry = new StandardServiceRegistryBuilder()
                .configure() // configures settings from hibernate.cfg.xml
                .build();
        try {
            sessionFactory = new MetadataSources( registry ).buildMetadata().buildSessionFactory();
            System.out.println("The session Factory has been created !");
        }
        catch (Exception e) {
            System.err.println("Exception "+e);
            StandardServiceRegistryBuilder.destroy( registry );
        }
    }

    static void close(){
        if ( sessionFactory != null ) {
            sessionFactory.close();
        }

    }

    public static void main(String[] args) {
        Main.main(args);
//        initialize();
//        IUserRepository userRepository = new UserHibernateRepository(sessionFactory, new UserValidator());
////        userRepository.save(new User("andrei","andrei",TypeUser.SECTION));
//        IOrderRepository orderRepository = new OrderHibernateRepository(sessionFactory, new OrderValidator());
//        //orderRepository.save(new Order(12, new User(1,"andrei","andrei", TypeUser.SECTION), Status.PENDING));
////        System.out.println(orderRepository.find(1));
////        orderRepository.update(new Order(1, 14, userRepository.find(1), Status.HONORED));
////        System.out.println(orderRepository.findAll());
//        IOrderItemRepository orderItemRepository = new OrderItemHibernateRepository(sessionFactory, new OrderItemValidator());
////        orderItemRepository.save(new OrderItem
////                (new Pair<Integer,Integer>(1,1),"ss",34, new Drug(1,"aa",34.4f,"s"),
////                        new Order(1, 14, userRepository.find(1), Status.HONORED)));
//      OrderItem orderItem  = orderItemRepository.find(new Pair<Integer, Integer>(1,1)) ;
//        orderItemRepository.update(new OrderItem
//                (new Pair<Integer,Integer>(1,1),"OPOP",34, new Drug(1,"aa",34.4f,"s"),
//                        new Order(1, 14, userRepository.find(1), Status.PENDING)));
//        System.out.println(orderItem);
//        System.out.println(orderItemRepository.findAll());
    }
}
