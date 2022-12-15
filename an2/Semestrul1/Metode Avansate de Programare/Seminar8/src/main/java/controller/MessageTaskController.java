package controller;


import domain.MessageTask;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.concurrent.Task;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Modality;
import javafx.stage.Stage;

import services.MessageTaskService;
import utils.events.MessageTaskChangeEvent;
import utils.events.TaskStatusEvent;
import utils.observer.Observer;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public class MessageTaskController implements Observer<MessageTaskChangeEvent> {
    MessageTaskService service;
    ObservableList<MessageTask> model = FXCollections.observableArrayList();


    @FXML
    TableView<MessageTask> tableView;
    @FXML
    TableColumn<MessageTask,String> tableColumnDesc;
    @FXML
    TableColumn<MessageTask,String> tableColumnFrom;
    @FXML
    TableColumn<MessageTask,String> tableColumnTo;
    @FXML
    TableColumn<MessageTask,String> tableColumnData;

    public void setMessageTaskService(MessageTaskService messageTaskService) {
        service = messageTaskService;
        service.addObserver(this);
        initModel();
    }

    @FXML
    public void initialize() {
        // TODO
    }

    private void initModel() {
        List<MessageTask> l = new ArrayList<>();
        service.getAll().forEach(l::add);
        ObservableList<MessageTask> tasks = FXCollections.observableArrayList(l);

        tableView.setItems(tasks);
        tableColumnData.setCellValueFactory(new PropertyValueFactory<>("date"));
        tableColumnFrom.setCellValueFactory(new PropertyValueFactory<>("from"));
        tableColumnTo.setCellValueFactory(new PropertyValueFactory<>("to"));
        tableColumnDesc.setCellValueFactory(new PropertyValueFactory<>("description"));
    }



    public void handleDeleteMessage(ActionEvent actionEvent) {
        // TODO
        MessageTask selectedItems = tableView.getSelectionModel().getSelectedItem();
        service.deleteMessageTask(selectedItems);

    }

    @Override
    public void update(MessageTaskChangeEvent messageTaskChangeEvent) {

        initModel();
    }

    @FXML
    public void handleUpdateMessage(ActionEvent ev) {
        // TODO
        MessageTask selectedItems = tableView.getSelectionModel().getSelectedItem();
        try {
            Stage stage = new Stage();
            stage.setTitle("Adauga mesaj");
            FXMLLoader messageLoader = new FXMLLoader();
            messageLoader.setLocation(getClass().getResource("/views/editMessageTaskView.fxml"));

            AnchorPane messageTaskLayout = messageLoader.load();

            EditMessageTaskController editMessageTaskController = messageLoader.getController();
            editMessageTaskController.setService(service, stage, selectedItems);


            stage.setScene(new Scene(messageTaskLayout));
            stage.setWidth(800);
            stage.show();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @FXML
    public void handleAddMessage(ActionEvent ev){
        try {
            Stage stage = new Stage();
            stage.setTitle("Adauga mesaj");
            FXMLLoader messageLoader = new FXMLLoader();
            messageLoader.setLocation(getClass().getResource("/views/editMessageTaskView.fxml"));

            AnchorPane messageTaskLayout = messageLoader.load();

            EditMessageTaskController editMessageTaskController = messageLoader.getController();
            editMessageTaskController.setService(service, stage, null);


            stage.setScene(new Scene(messageTaskLayout));
            stage.setWidth(800);
            stage.show();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        showMessageTaskEditDialog(null);
    }

    public void showMessageTaskEditDialog(MessageTask messageTask) {
        // TODO

    }



}
