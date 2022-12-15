package com.example.guiex1;

import com.example.guiex1.domain.Utilizator;
import javafx.beans.Observable;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.ObservableListBase;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;

import java.util.Arrays;
import java.util.List;

public class HelloController {
    @FXML
    private Label welcomeText;

    @FXML
    private Button btnHello;

    @FXML
    ListView<Utilizator> listView;

    @FXML
    public void onHelloButtonClick() {
        welcomeText.setText("Welcome to JavaFX Application!");
        btnHello.setText("alt text");
        List<Utilizator> list = Arrays.asList(new Utilizator("dan","ana"));
        ObservableList<Utilizator> observableList= FXCollections.observableList(list);
        listView.setItems(observableList);
    }
}