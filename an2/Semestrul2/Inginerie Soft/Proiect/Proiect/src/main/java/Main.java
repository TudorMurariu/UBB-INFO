import controllers.LoginController;
import domain.*;
import domain.validators.*;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.util.Pair;
import org.hibernate.SessionFactory;
import org.hibernate.boot.MetadataSources;
import org.hibernate.boot.registry.StandardServiceRegistry;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import org.hibernate.cfg.Configuration;
import repository.*;
import services.Services;

import java.io.IOException;
import java.util.Properties;

public class Main extends Application{

    private static final int INSTANCES = 3;
    private static int thereMore = 0;
    private static Services services = null;

    static SessionFactory sessionFactory;
    static void initialize() {
//        Configuration configuration = new Configuration();
//        configuration.configure();
//        StandardServiceRegistryBuilder registryBuilder = new StandardServiceRegistryBuilder()
//                .applySettings(configuration.getProperties());
//        sessionFactory = configuration.buildSessionFactory(registryBuilder.build());

        // A SessionFactory is set up once for an application
        final StandardServiceRegistry registry = new StandardServiceRegistryBuilder()
                .configure() // configures settings from hibernate.cfg.xml
                .build();
        try {
            // sessionFactory = configuration.buildSessionFactory(registryBuilder.build());
            sessionFactory = new MetadataSources( registry ).buildMetadata().buildSessionFactory();
            System.out.println("The session factory has been created!");
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

    private static Services getServices(){
        if(services == null){
            Properties serverProps = new Properties();
            try{
                serverProps.load(Main.class.getResourceAsStream("/travelServer.properties"));
                System.out.println("Server properties set. ");
                serverProps.list(System.out);
            } catch (IOException e) {
                System.err.println("Cannot find travelserver.properties" + e);
                return null;
            }

            Validator<User> userValidator = new UserValidator();
            Validator<Drug> drugValidator = new DrugValidator();
            Validator<Order> orderValidator = new OrderValidator();
            Validator<OrderItem> orderItemValidator = new OrderItemValidator();

            initialize();
            IUserRepository userRepository = new UserHibernateRepository(sessionFactory, userValidator);
            IDrugRepository drugRepository = new DrugHibernateRepository(sessionFactory, drugValidator);
            IOrderRepository orderRepository = new OrderHibernateRepository(sessionFactory, orderValidator);
            IOrderItemRepository orderItemRepository = new OrderItemHibernateRepository(sessionFactory, orderItemValidator);

            orderItemRepository.save(new OrderItem
                    (new Pair<Integer,Integer>(3, 11), "CONTROLOC", 10000, drugRepository.find(3),
                            orderRepository.find(11)));

            System.out.println(orderItemRepository.findAll());
            services = new Services(userRepository, drugRepository, orderRepository,orderItemRepository);
        }
        return services;
    }

    @Override
    public void start(Stage stage) throws IOException {

        for(int i = 0 ; i < INSTANCES ; i++) {
            try {
                if(Main.thereMore < INSTANCES) {
                    runAnotherApp(Main.class);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

    }

    public void runAnotherApp(Class<? extends Application> anotherAppClass) throws Exception {
        Main.thereMore ++;

        Application app2 = anotherAppClass.newInstance();
        Stage anotherStage = new Stage();
        FXMLLoader fxmlLoader = new FXMLLoader();
        fxmlLoader.setLocation(getClass().getClassLoader().getResource("./views/LogIn.fxml"));
        Scene scene = new Scene(fxmlLoader.load());

        anotherStage.setTitle("Hospital");
        anotherStage.setScene(scene);
        LoginController loginController = fxmlLoader.getController();
        loginController.setServices(anotherStage, services);
        anotherStage.show();
        app2.start(anotherStage);
    }

    public static void main(String[] args) {
        getServices();
        launch();
    }
}
