package repository;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import domain.MessageTask;
import domain.validators.Validator;
import utils.Constants;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.time.LocalDateTime;

public class XMLMessageTaskRepository extends InMemoryRepository<String, MessageTask> {
    private String fileName;

    public XMLMessageTaskRepository(Validator<MessageTask> validator, String fileName) {
        super(validator);
        this.fileName = fileName;
        loadData();
    }

    private void loadData() {
        try {
            Document document = DocumentBuilderFactory
                    .newInstance()
                    .newDocumentBuilder()
                    .parse(this.fileName);

            Element root = document.getDocumentElement();
            NodeList children = root.getChildNodes();
            for(int i=0; i < children.getLength(); i++) {
                Node messageTaskElement = children.item(i);
                if(messageTaskElement instanceof Element) {
                    MessageTask messageTask = createMessageTaskFromElement((Element)messageTaskElement);
                    super.save(messageTask);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void writeToFile(){
        try {
            Document document = DocumentBuilderFactory
                    .newInstance()
                    .newDocumentBuilder()
                    .newDocument();
            Element root  = document.createElement("inbox");
            document.appendChild(root);
            super.findAll().forEach(m->{
                Element e = createElementfromMessageTask(document,m);
                root.appendChild(e);
            });

            //write Document to file
            Transformer transformer = TransformerFactory.
                    newInstance().newTransformer();
            transformer.transform(new DOMSource(document),
                    new StreamResult(fileName));

        }catch(Exception e){
            e.printStackTrace();
        }
    }

    private Element createElementfromMessageTask(Document document, MessageTask m) {
        Element e = document.createElement("messageTask");
        e.setAttribute("taskid", m.getId());

        Element desc = document.createElement("description");
        desc.setTextContent(m.getDescription());
        e.appendChild(desc);

        Element message = document.createElement("message");
        message.setTextContent(m.getMessage());
        e.appendChild(message);

        Element from = document.createElement("from");
        from.setTextContent(m.getFrom());
        e.appendChild(from);

        Element to = document.createElement("to");
        to.setTextContent(m.getTo());
        e.appendChild(to);

        Element date = document.createElement("date");
        date.setTextContent(m.getDate().format(Constants.DATE_TIME_FORMATTER));
        e.appendChild(date);

        return e;
    }

    @Override
    public MessageTask save(MessageTask entity) {
        MessageTask stuff;
        stuff = super.save(entity);
        if (stuff == null){
            writeToFile();
        }
        return stuff;
    }

    private MessageTask createMessageTaskFromElement(Element messageTaskElement) {
        String taskId = messageTaskElement.getAttribute("taskid");
        NodeList nods = messageTaskElement.getChildNodes();
       String desc =messageTaskElement.getElementsByTagName("description")
               .item(0)
               .getTextContent();

        String message =messageTaskElement.getElementsByTagName("message")
                .item(0)
                .getTextContent();

        String from =messageTaskElement.getElementsByTagName("from")
                .item(0)
                .getTextContent();

        String to =messageTaskElement.getElementsByTagName("to")
                .item(0)
                .getTextContent();

        String date =messageTaskElement.getElementsByTagName("date")
                .item(0)
                .getTextContent();
        return new MessageTask(taskId,desc,message,from,to,LocalDateTime.parse(date, Constants.DATE_TIME_FORMATTER));

    }


}
