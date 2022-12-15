
import domain.MessageTask;
import domain.validators.MessageTaskValidator;
import repository.CrudRepository;
import repository.InFileMessageTaskRepository;
import repository.paging.PagingRepository;
import services.MessageTaskService;

public class Main {
    public static void main(String[] args) {
        PagingRepository<String, MessageTask> messageTaskRepository;
        MessageTaskService service;

        messageTaskRepository = new InFileMessageTaskRepository
                ("data/messages.txt", new MessageTaskValidator());
        service=new MessageTaskService(messageTaskRepository);
        //service.getAll().forEach(System.out::println);
        service.setPageSize(2);

        System.out.println("Elements on page 2");
        service.getMessagesOnPage(2).stream()
                .forEach(System.out::println);
        System.out.println("Elements on next page");
        service.getNextMessages().stream()
                .forEach(System.out::println);

       MainApp.main(args);
      }
}
