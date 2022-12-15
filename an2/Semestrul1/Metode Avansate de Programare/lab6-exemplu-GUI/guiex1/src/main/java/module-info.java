module com.example.guiex1 {
    requires javafx.controls;
    requires javafx.fxml;

    requires org.controlsfx.controls;
    requires com.dlsc.formsfx;
    requires org.kordamp.bootstrapfx.core;
    requires java.sql;

    opens com.example.guiex1 to javafx.fxml;
    opens com.example.guiex1.controller to javafx.fxml;

    exports com.example.guiex1;
    exports com.example.guiex1.domain;
    exports com.example.guiex1.controller;
}