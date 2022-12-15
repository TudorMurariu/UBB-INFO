package repository;

import domain.MessageTask;
import domain.validators.Validator;
import utils.Constants;

import java.time.LocalDateTime;
import java.util.List;

public class InFileMessageTaskRepository extends AbstractFileRepository<String, MessageTask> {

    public InFileMessageTaskRepository(String fileName, Validator<MessageTask> validator) {
        super(fileName, validator);
    }

    @Override
    public MessageTask extractEntity(List<String> attr) {
       //return Messages.createMessageTask(attributes);
        String id=attr.get(0).split("=")[1];
        String desc=attr.get(1).split("=")[1];
        String msg=attr.get(2).split("=")[1];
        String from=attr.get(3).split("=")[1];
        String to=attr.get(4).split("=")[1];
        String dateAsString=attr.get(5).split("=")[1];
        LocalDateTime date= LocalDateTime.parse(dateAsString, Constants.DATE_TIME_FORMATTER);
        MessageTask t=new MessageTask(id,desc,msg,from,to,date);
        return t;
    }

    @Override
    public String createStringEntity(MessageTask x) {
        //id=1212|description=mesaj de la radu|message=ce faci?|from=radu|to=andrei|date=2018-11-18 22:09
        String res="";
        res+="id="+x.getId()+"|description="+x.getDescription()+"|message="+x.getMessage()
                +"|from="+x.getFrom()+"|to="+x.getTo()
                +"|date="+x.getDate().format(Constants.DATE_TIME_FORMATTER);
        return res;
    }
}


