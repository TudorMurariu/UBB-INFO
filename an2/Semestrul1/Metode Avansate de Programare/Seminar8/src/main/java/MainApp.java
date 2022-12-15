import controller.MessageTaskController;
import domain.MessageTask;
import domain.validators.MessageTaskValidator;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;
import repository.CrudRepository;
import repository.InFileMessageTaskRepository;
import repository.paging.PagingRepository;
import services.MessageTaskService;


import java.io.IOException;

public class MainApp extends Application {

    PagingRepository<String, MessageTask> messageTaskRepository;
    MessageTaskService messageTaskService;

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) throws IOException {
        String fileN = "data/messages.txt";
        messageTaskRepository = new InFileMessageTaskRepository
                (fileN, new MessageTaskValidator());
        messageTaskService = new MessageTaskService(messageTaskRepository);
        //messageTaskService.getAll().forEach(System.out::println);

        initView(primaryStage);
        primaryStage.setWidth(800);
        primaryStage.show();

    }

    private void initView(Stage primaryStage) throws IOException {

        FXMLLoader messageLoader = new FXMLLoader();
        messageLoader.setLocation(getClass().getResource("/views/messageTaskView.fxml"));
        AnchorPane messageTaskLayout = messageLoader.load();
        primaryStage.setScene(new Scene(messageTaskLayout));

        MessageTaskController messageTaskController = messageLoader.getController();
        messageTaskController.setMessageTaskService(messageTaskService);

    }
}
