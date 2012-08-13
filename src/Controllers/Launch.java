package com.jamesrthompson.Controllers;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Launch extends Application {

    public static void main(String[] args) {
        Application.launch(Launch.class, args);
    }

    @Override
    public void start(Stage stage) throws Exception {
        try {
            URL location = getClass().getResource("SMMainStage.fxml");
            System.out.println(location.toString());
            FXMLLoader loader = new FXMLLoader(location);
            AnchorPane page = (AnchorPane)loader.load();
            Scene scene = new Scene(page);
            stage.setScene(scene);
            stage.initStyle(StageStyle.UTILITY);
            stage.setTitle("Single Molecule Tracker");
            stage.show();
        } catch (Exception ex) {
            Logger.getLogger(Launch.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}