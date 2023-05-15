package controllers;

import domain.TypeUser;
import domain.User;
import domain.validators.ValidationException;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.stage.Stage;
import services.Services;

import java.io.IOException;
import java.lang.reflect.Type;

public class LoginController {

    Stage mainStage;
    Services services;
    TypeUser typeUser;
    TypeUser typeUserSignup;
    User rootUser;

    @FXML
    TextField loginUsernameText;
    @FXML
    TextField loginPasswordText;
    @FXML
    TextField signupUsernameText;
    @FXML
    TextField signupPasswordText;
    @FXML
    TextField signupConfirmPasswordText;
    @FXML
    Button loginBut;
    @FXML
    Button signupBut;
    @FXML
    RadioButton pharmacyRadioLogin;
    @FXML
    RadioButton sectionRadioLogin;
    @FXML
    RadioButton adminRadioLogin;
    @FXML
    RadioButton pharmacyRadioSignup;
    @FXML
    RadioButton sectionRadioSignup;
    @FXML
    RadioButton adminRadioSignup;

    public void setServices(Stage primaryStage, Services service){
        this.mainStage = primaryStage;
        this.services = service;
    }

    @FXML
    public void initialize(){

    }

    @FXML
    void handleLogin() throws IOException {
        String username = loginUsernameText.getText();
        String password = loginPasswordText.getText();

        User rootUser = new User(username, password, typeUser);
        try{
            this.rootUser = services.login(rootUser);

            if(typeUser.equals(TypeUser.SECTION)) {
                FXMLLoader loader = new FXMLLoader();
                loader.setLocation(getClass().getResource("/views/Section.fxml"));
                Parent root = loader.load();
                Scene newScene = new Scene(root);
                mainStage.setScene(newScene);
                SectionController sectionController = loader.getController();
                sectionController.setServices(mainStage, services, this.rootUser);
                mainStage.show();
            }
            else
            if(typeUser.equals(TypeUser.PHARMACY)) {
                FXMLLoader loader = new FXMLLoader();
                loader.setLocation(getClass().getResource("/views/Pharmacy.fxml"));
                Parent root = loader.load();
                Scene newScene = new Scene(root);
                mainStage.setScene(newScene);
                PharmacyController pharmacyController = loader.getController();
                pharmacyController.setServices(mainStage, services, this.rootUser);
                mainStage.show();
            }
            else
            if(typeUser.equals(TypeUser.ADMIN)) {
                FXMLLoader loader = new FXMLLoader();
                loader.setLocation(getClass().getResource("/views/MedsManagement.fxml"));
                Parent root = loader.load();
                Scene newScene = new Scene(root);
                mainStage.setScene(newScene);
                AdminController adminController = loader.getController();
                adminController.setServices(mainStage, services, this.rootUser);
                mainStage.show();
            }
        }
        catch (ValidationException ex){
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setTitle("Travel Agency");
            alert.setHeaderText("Authentication failure");
            alert.setContentText(ex.getMessage());
            alert.showAndWait();
            clearLogin();
        }
    }

    @FXML
    void handleSignup(){
        String username = signupUsernameText.getText();
        String password = signupPasswordText.getText();
        String confirmPassword = signupConfirmPasswordText.getText();
        if(!confirmPassword.equals(password)){
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setTitle("Travel Agency");
            alert.setHeaderText("Authentication failure");
            alert.setContentText("Password doesn't match !");
            alert.showAndWait();
            signupUsernameText.clear();;
            signupPasswordText.clear();
            signupConfirmPasswordText.clear();
            clearSignup();
            return;
        }
        try{
            User signupUser = new User(username, password, typeUserSignup);
            services.signUp(signupUser);
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setTitle("Hospital");
            alert.setHeaderText("Registration");
            alert.setContentText("The user was registered !");
            alert.showAndWait();
            clearSignup();
        }catch (ValidationException ex){
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setTitle("Travel Agency");
            alert.setHeaderText("Authentication failure");
            alert.setContentText(ex.getMessage());
            alert.showAndWait();
            clearSignup();
        }
    }

    private void clearLogin(){
        loginUsernameText.clear();
        loginPasswordText.clear();
    }

    private void clearSignup(){
        signupUsernameText.clear();
        signupPasswordText.clear();
        signupConfirmPasswordText.clear();
    }

    @FXML
    void handlePharmacyRadioLogin(){
        typeUser = TypeUser.PHARMACY;
    }

    @FXML
    void handleSectionRadioLogin(){
        typeUser = TypeUser.SECTION;
    }

    @FXML
    void handleAdminRadoLogin(){
        typeUser = TypeUser.ADMIN;
    }

    @FXML
    void handlePharmacyRadioSignup(){
        typeUserSignup = TypeUser.PHARMACY;
    }

    @FXML
    void handleSectionRadioSignup(){
        typeUserSignup = TypeUser.SECTION;
    }

    @FXML
    void handleAdminRadoSignup(){
        typeUserSignup = TypeUser.ADMIN;
    }
}
