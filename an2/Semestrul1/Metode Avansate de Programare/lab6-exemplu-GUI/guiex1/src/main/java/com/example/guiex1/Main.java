package com.example.guiex1;

import com.example.guiex1.domain.Utilizator;
import com.example.guiex1.domain.UtilizatorValidator;
import com.example.guiex1.repository.Repository;
import com.example.guiex1.repository.dbrepo.UtilizatorDbRepository;


public class Main {
    public static void main(String[] args) {
        System.out.println("ok");
       //final String fileName=ApplicationContext.getPROPERTIES().getProperty("data.socialnetwork.users");
//        String fileName="data/users.csv";
//        Repository0<Long,Utilizator> userFileRepository = new UtilizatorFile0(fileName
//                , new UtilizatorValidator());
//
//        System.out.println("Reading data from file");
//        Repository<Long,Utilizator> userFileRepository2 = new UtilizatorFile(fileName
//                , new UtilizatorValidator());
//        userFileRepository2.findAll().forEach(x-> System.out.println(x));
////
//        System.out.println("Reading data from database");
//        final String url = ApplicationContext.getPROPERTIES().getProperty("database.socialnetwork.url");
//        final String username= ApplicationContext.getPROPERTIES().getProperty("databse.socialnetwork.username");
//        final String pasword= ApplicationContext.getPROPERTIES().getProperty("database.socialnetwork.pasword");
        System.out.println("Reading data from file");
        String username="postgres";
        String pasword="postgres";
        String url="jdbc:postgresql://localhost:5434/socialnetwork";
        Repository<Long, Utilizator> userFileRepository3 =
                new UtilizatorDbRepository(url,username, pasword,  new UtilizatorValidator());

        userFileRepository3.findAll().forEach(x-> System.out.println(x));

    }
}


