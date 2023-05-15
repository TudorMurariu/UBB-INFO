package controllers;

import domain.*;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;

import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.stage.Stage;
import javafx.util.Pair;
import services.Services;
import utils.MessageAlert;
import utils.events.Event;
import utils.events.MedsManagementEvent;
import utils.observer.Observer;

import java.io.IOException;


public class SectionController implements Observer<Event> {

    Stage mainStage;
    User rootUser;
    Services services;

    ObservableList<Drug> modelAllDrugs = FXCollections.observableArrayList();
    ObservableList<OrderItem> modelOrderItems = FXCollections.observableArrayList();

    @FXML
    TableView<Drug> tableViewDrugs;
    @FXML
    TableColumn<Drug, String> tableColumnName;
    @FXML
    TableColumn<Drug, Float> tableColumnPrice;
    @FXML
    TableColumn<Drug, String> tableColumnDescription;
    @FXML
    TextField quantityTextField;

    @FXML
    TableView<OrderItem> tableOrderItems;
    @FXML
    TableColumn<OrderItem, String> tableColumnOrderItemName;
    @FXML
    TableColumn<OrderItem, Integer> tableColumnOrderItemQuantity;



    public void setServices(Stage primaryStage, Services service, User rootUser){
        this.mainStage = primaryStage;
        this.services = service;
        this.rootUser = rootUser;
        service.addObserver(this);
        initModelAllDrugs();
    }

    @FXML
    void initialize(){
        tableColumnName.setCellValueFactory(new PropertyValueFactory<Drug, String >("name"));
        tableColumnPrice.setCellValueFactory(new PropertyValueFactory<Drug, Float>("price"));
        tableColumnDescription.setCellValueFactory(new PropertyValueFactory<Drug, String>("description"));
        tableViewDrugs.setItems(modelAllDrugs);

        tableColumnOrderItemName.setCellValueFactory(new PropertyValueFactory<OrderItem, String>("drugName"));
        tableColumnOrderItemQuantity.setCellValueFactory(new PropertyValueFactory<OrderItem, Integer>("quantity"));
        tableOrderItems.setItems(modelOrderItems);
    }

    private void initModelAllDrugs(){
        services.findAllDrugs().stream().forEach(System.out::println);
        modelAllDrugs.setAll(services.findAllDrugs());
    }

    @Override
    public void update(Event event) {
        if(event instanceof MedsManagementEvent)
            initModelAllDrugs();
    }

    @FXML
    private void handleAddOrderDrug(){
        Drug drug = tableViewDrugs.getSelectionModel().getSelectedItem();
        if(drug == null)
            return;
        try{
            Integer quantity = Integer.valueOf(quantityTextField.getText());
            OrderItem orderItem = new OrderItem(new Pair<>(drug.getID(), null), drug.getName(), quantity, drug, null);
            modelOrderItems.add(orderItem);
        }catch (RuntimeException ex){
            MessageAlert.showErrorMessage(mainStage, ex.getMessage());
        }
    }

    @FXML
    private void handleAddOrder(){
        Integer totalQuantity = 0;
        for(OrderItem orderItem: modelOrderItems) {
            System.out.println("LLLLLLLLLLLLLPPPPPPPPPPPPP: " + orderItem);
            totalQuantity += orderItem.getQuantity();
        }
        try{
            Order order = new Order(totalQuantity, rootUser, Status.PENDING);
            services.addOrder(order, modelOrderItems);
            MessageAlert.showMessage(mainStage, Alert.AlertType.INFORMATION, "Add Order", "The order was added successfully !");
        }catch (RuntimeException ex){
            MessageAlert.showErrorMessage(mainStage, ex.getMessage());
        }
    }

    @FXML
    private void handleRemoveOrderItem(){
        OrderItem orderItem = tableOrderItems.getSelectionModel().getSelectedItem();
        if(orderItem == null)
            return;
        modelOrderItems.remove(orderItem);
    }

    @FXML
    private void handleYourOrders() throws IOException {
        FXMLLoader loader = new FXMLLoader();
        loader.setLocation(getClass().getResource("/views/ViewOrdersSection.fxml"));
        Parent root = loader.load();
        Scene newScene = new Scene(root);
        Scene previousScene = mainStage.getScene();
        mainStage.setScene(newScene);
        OrderSectionController orderSectionController = loader.getController();
        orderSectionController.setServices(mainStage, services, this.rootUser, previousScene);
        mainStage.show();
    }

    @FXML
    private void handleLogout(){
        try{
            services.logout(rootUser);
            mainStage.close();
        }catch (RuntimeException ex){
            MessageAlert.showErrorMessage(mainStage, ex.getMessage());
        }
    }

}
