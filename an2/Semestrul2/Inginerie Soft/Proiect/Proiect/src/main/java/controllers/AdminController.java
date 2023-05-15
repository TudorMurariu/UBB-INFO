package controllers;

import domain.Drug;
import domain.User;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.stage.Stage;
import services.Services;
import utils.MessageAlert;
import utils.events.Event;
import utils.events.MedsManagementEvent;
import utils.observer.Observable;
import utils.observer.Observer;

import javax.xml.bind.ValidationException;
import java.util.Locale;

public class AdminController implements Observer<Event> {

    Stage mainStage;
    User rootUser;
    Services services;

    ObservableList<Drug> modelAllDrugs = FXCollections.observableArrayList();

    @FXML
    TableView<Drug> tableViewDrugs;
    @FXML
    TableColumn<Drug, String> tableColumnName;
    @FXML
    TableColumn<Drug, Float> tableColumnPrice;
    @FXML
    TableColumn<Drug, String> tableColumnDescription;
    @FXML
    TextField nameTextField;
    @FXML
    TextField priceTextField;
    @FXML
    TextField descriptionTextField;

    public void setServices(Stage primaryStage, Services service, User rootUser){
        this.mainStage = primaryStage;
        this.services = service;
        this.rootUser = rootUser;
        initModelAllDrugs();
        service.addObserver(this);
    }

    @FXML
    void initialize(){

        tableColumnName.setCellValueFactory(new PropertyValueFactory<Drug, String >("name"));
        tableColumnPrice.setCellValueFactory(new PropertyValueFactory<Drug, Float>("price"));
        tableColumnDescription.setCellValueFactory(new PropertyValueFactory<Drug, String>("description"));
        tableViewDrugs.setItems(modelAllDrugs);

        tableViewDrugs.getSelectionModel().selectedItemProperty().addListener((obs, oldVal, newVal) -> {
            if(newVal != null){
                nameTextField.setText(newVal.getName());
                priceTextField.setText(newVal.getPrice().toString());
                descriptionTextField.setText(newVal.getDescription());
            }
        });
    }

    private void initModelAllDrugs(){
        modelAllDrugs.setAll(services.findAllDrugs());
    }

    @Override
    public void update(Event event) {
        if(event instanceof MedsManagementEvent)
            initModelAllDrugs();
    }

    @FXML
    void handleAddDrug(){
        try{
            String name = nameTextField.getText();
            Float price = Float.valueOf(priceTextField.getText());
            String description = descriptionTextField.getText();
            Drug addedDrug = new Drug(name, price, description);
            services.addDrug(addedDrug);
            MessageAlert.showMessage(mainStage, Alert.AlertType.INFORMATION, "Add Drug", "The drug was added successfully !");

        }catch (RuntimeException ex){
            MessageAlert.showErrorMessage(mainStage, ex.getMessage());
        }
    }

    @FXML
    void handleUpdateDrug(){
        Drug updetedDrug = tableViewDrugs.getSelectionModel().getSelectedItem();
        if(updetedDrug == null)
            return;
        try{
            String name = nameTextField.getText();
            Float price = Float.valueOf(priceTextField.getText());
            String description = descriptionTextField.getText();
            Drug updatedDrug = new Drug(updetedDrug.getID(), name, price, description);
            services.updateDrug(updatedDrug);
            MessageAlert.showMessage(mainStage, Alert.AlertType.INFORMATION, "Update Drug", "The drug was updated successfully !");
        }catch (RuntimeException ex){
            MessageAlert.showErrorMessage(mainStage, ex.getMessage());
        }
    }

    @FXML
    void handleDeleteDrug(){
        Drug deletedDrug = tableViewDrugs.getSelectionModel().getSelectedItem();
        System.out.println(deletedDrug);
        if(deletedDrug == null)
            return;
        try{
            services.deleteDrug(deletedDrug);
            MessageAlert.showMessage(mainStage, Alert.AlertType.INFORMATION, "Delete Drug", "The drug was deleted successfully !");
        }catch (RuntimeException ex){
            MessageAlert.showErrorMessage(mainStage, ex.getMessage());
        }
    }

    @FXML
    void handleLogOut(){
        try{
            services.logout(rootUser);
            mainStage.close();
        }catch (RuntimeException ex){
            MessageAlert.showErrorMessage(mainStage, ex.getMessage());
        }
    }

}
