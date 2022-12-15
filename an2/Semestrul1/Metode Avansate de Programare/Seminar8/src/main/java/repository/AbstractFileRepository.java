package repository;

import domain.Entity;
import domain.validators.Validator;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.List;

public abstract class AbstractFileRepository<ID, E extends Entity<ID>> extends InMemoryRepository<ID,E> {
    String fileName;
    public AbstractFileRepository(String fileName, Validator<E> validator) {
        super(validator);
        this.fileName=fileName;
        loadData();

    }
    public abstract E extractEntity(List<String> attributes);
    public abstract String createStringEntity(E x);

    private void loadData() {

        Path path = Paths.get(fileName);
        try {
            List<String> lines = Files.readAllLines(path);
            lines.forEach(linie -> {
                List<String> attr=Arrays.asList(linie.split("\\|"));
                E e=extractEntity(attr);
                E res=super.save(e);
            });
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public E save(E entity){
        E e=super.save(entity);
        if (e==null)
        {
            writeData();
        }
        return e;
    }

    @Override
    public E update(E entity){
        E e=super.update(entity);
        if (e==null)
        {
            writeData();
        }
        return e;
    }

    private void writeData() {
        Path path = Paths.get(fileName);
        try (BufferedWriter bW = Files.newBufferedWriter(path, StandardOpenOption.WRITE)) {
            findAll().forEach(x->{
                try {
                    bW.write(createStringEntity(x));
                    bW.newLine();
                } catch (IOException e) {
                    e.printStackTrace();
                }

            });
        } catch (IOException e) {
            e.printStackTrace();
        }
    }




}
