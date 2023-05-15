package controllers;

import domain.*;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.*;
import javafx.fxml.FXML;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.util.Pair;
import services.Services;
import utils.events.Event;
import utils.events.MedsManagementEvent;
import utils.events.OrderEvent;
import utils.observer.Observer;

import java.util.Collection;
import java.util.stream.Collectors;


public class OrderSectionController implements Observer<Event> {

    Stage mainStage;
    Scene previousScene;
    User rootUser;
    Services services;
    ObservableList<ComboBox<OrderItemDTO>> modelYoursOrders = FXCollections.observableArrayList();
    ObservableList<ComboBox<OrderItemDTO>> modelMadeOrders = FXCollections.observableArrayList();

    @FXML
    ListView<ComboBox<OrderItemDTO>> listViewYoursOrders;

    @FXML
    ListView<ComboBox<OrderItemDTO>> listViewMadeOrders;

    public void setServices(Stage primaryStage, Services service, User rootUser, Scene previousScene){
        this.mainStage = primaryStage;
        this.services = service;
        this.rootUser = rootUser;
        this.previousScene = previousScene;
        service.addObserver(this);
        initModelAllOrders();
    }

    @FXML
    void initialize(){
        listViewYoursOrders.setItems(modelYoursOrders);
        listViewMadeOrders.setItems(modelMadeOrders);
    }

    @Override
    public void update(Event event) {
        if(event instanceof MedsManagementEvent || event instanceof OrderEvent)
            initModelAllOrders();
    }

    private void initModelAllOrders(){
        modelYoursOrders.clear();
        Collection<Pair<Order, Collection<OrderItem>>> orderItems = services.getAllOrderForSection(rootUser);
        orderItems.forEach(orderCollectionPair -> {
            Order order = orderCollectionPair.getKey();
            Collection<OrderItemDTO> orderItemCollection = orderCollectionPair.getValue().stream()
                    .map(orderItem -> {
                        String name = orderItem.getDrugName();
                        Integer price = orderItem.getQuantity();
                        Status status = orderItem.getOrder().getStatus();
                        return new OrderItemDTO(name, price, status);
                    }).collect(Collectors.toList());
            ObservableList<OrderItemDTO> modelOrderItems = FXCollections.observableArrayList();
            ComboBox<OrderItemDTO> comboBoxSpecificOrder = new ComboBox<>();
            comboBoxSpecificOrder.setPromptText("Status: " + order.getStatus() + " Total Quantity: " + order.getQuantity());
            comboBoxSpecificOrder.setItems(modelOrderItems);
            modelOrderItems.setAll(orderItemCollection);
            modelYoursOrders.add(comboBoxSpecificOrder);
        });

    }

    private void filterOrdersByStatus(Status statusReceived){
        modelMadeOrders.clear();
        Collection<Pair<Order, Collection<OrderItem>>> orderItemsOrders = services.filterOrdersByStatus(statusReceived);
        orderItemsOrders.forEach(orderItemsOrder -> {
                    Order order = orderItemsOrder.getKey();
                    Collection<OrderItem> orderItems = orderItemsOrder.getValue();
                    Collection<OrderItemDTO> orderItemDTOS = orderItems.stream()
                            .map(orderItem -> {
                                String name = orderItem.getDrugName();
                                Integer price = orderItem.getQuantity();
                                Status status = orderItem.getOrder().getStatus();
                                return new OrderItemDTO(name, price, status);
                            }).collect(Collectors.toList());

                    ObservableList<OrderItemDTO> modelOrderItems = FXCollections.observableArrayList();
                    ComboBox<OrderItemDTO> comboBox = new ComboBox<OrderItemDTO>();
                    comboBox.setPromptText("Status: " + order.getStatus() + " Total Quantity: " + order.getQuantity());
                    comboBox.setItems(modelOrderItems);
                    modelOrderItems.setAll(orderItemDTOS);
                    modelMadeOrders.add(comboBox);
                });
    }

    @FXML
    private void filterRefusedOrders(){
        filterOrdersByStatus(Status.REFUSED);
    }

    @FXML
    private void filterPendingOrders(){
        filterOrdersByStatus(Status.PENDING);
    }

    @FXML
    private void filterHonoredOrders(){
        filterOrdersByStatus(Status.HONORED);
    }

    @FXML
    void handleQuit(){
        mainStage.setScene(previousScene);
    }
}
