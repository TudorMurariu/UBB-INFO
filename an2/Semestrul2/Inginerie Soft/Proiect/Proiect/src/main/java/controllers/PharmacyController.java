package controllers;

import domain.Order;
import domain.OrderItem;
import domain.Status;
import domain.User;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Alert;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.stage.Stage;
import services.Services;
import utils.MessageAlert;
import utils.events.Event;
import utils.events.MedsManagementEvent;
import utils.events.OrderEvent;
import utils.observer.Observer;

import java.util.Collection;

public class PharmacyController implements Observer<Event> {

    Stage mainStage;
    User rootUser;
    Services services;

    ObservableList<Order> modelAllOrders = FXCollections.observableArrayList();
    ObservableList<OrderItem> modelOrderItems = FXCollections.observableArrayList();

    @FXML
    TableView<Order> tableViewOrders;
    @FXML
    TableColumn<Order, User> tableColumnSection;
    @FXML
    TableColumn<Order, Integer> tableColumnQuantity;
    @FXML
    TableColumn<Order, Status> tableColumnStatus;

    @FXML
    TableView<OrderItem> tableViewOrderItem;
    @FXML
    TableColumn<OrderItem, String> tableColumnOrderItemName;
    @FXML
    TableColumn<OrderItem, Integer> tableColumnOrderItemQuantity;


    public void setServices(Stage primaryStage, Services service, User rootUser){
        this.mainStage = primaryStage;
        this.services = service;
        this.rootUser = rootUser;
        service.addObserver(this);
        updateOrderList();
    }

    @FXML
    void initialize(){
        tableColumnSection.setCellValueFactory(new PropertyValueFactory<Order, User>("user"));
        tableColumnQuantity.setCellValueFactory(new PropertyValueFactory<Order, Integer>("quantity"));
        tableColumnStatus.setCellValueFactory(new PropertyValueFactory<Order, Status>("status"));
        tableViewOrders.setItems(modelAllOrders);

        tableColumnOrderItemName.setCellValueFactory(new PropertyValueFactory<OrderItem, String>("drugName"));
        tableColumnOrderItemQuantity.setCellValueFactory(new PropertyValueFactory<OrderItem, Integer>("quantity"));
        tableViewOrderItem.setItems(modelOrderItems);

        tableViewOrders.getSelectionModel().selectedItemProperty().addListener((obs, oldVal, newVal) -> {
            if(newVal != null){
                Collection<OrderItem> orderItems = services.getAllOrderItemsForOrder(newVal);
                modelOrderItems.setAll(orderItems);
            }
        });

    }

    private void updateOrderList(){
        Collection<Order> orders = services.getOrdersByStatus(Status.PENDING);
        modelAllOrders.setAll(orders);
    }

    @Override
    public void update(Event event) {
        if(event instanceof MedsManagementEvent || event instanceof OrderEvent)
            updateOrderList();
    }

    @FXML
    private void handleHonorOrder(){
        Order order = tableViewOrders.getSelectionModel().getSelectedItem();
        if(order == null)
            return;
        try{
            services.honorOrder(order);
            MessageAlert.showMessage(mainStage, Alert.AlertType.INFORMATION, "Honor Order", "The order was honored!");
        }catch (RuntimeException ex){
            MessageAlert.showErrorMessage(mainStage, ex.getMessage());
        }
    }

    @FXML
    private void handleRejectOrder(){
        Order order = tableViewOrders.getSelectionModel().getSelectedItem();
        if(order == null)
            return;
        try{
            services.refuseOrder(order);
            MessageAlert.showMessage(mainStage, Alert.AlertType.INFORMATION, "Refuse Order", "The order was rejected!");
        }catch (RuntimeException ex){
            MessageAlert.showErrorMessage(mainStage, ex.getMessage());
        }
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
