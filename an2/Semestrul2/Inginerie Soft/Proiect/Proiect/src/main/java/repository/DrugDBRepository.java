package repository;

import domain.Drug;
import domain.User;
import domain.validators.Validator;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;

public class DrugDBRepository implements IDrugRepository{

    private JdbcUtils jdbcUtils;
    private Validator<Drug> validator;

    public DrugDBRepository(Properties properties, Validator<Drug> validator) {
        this.validator = validator;
        jdbcUtils = new JdbcUtils(properties);
    }

    @Override
    public void save(Drug elem) {
        validator.validate(elem);
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement insertStmt = connection
                    .prepareStatement("insert into drugs(name,price,description) values (?,?,?)")) {
            insertStmt.setString(1, elem.getName());
            insertStmt.setFloat(2, elem.getPrice());
            insertStmt.setString(3, elem.getDescription());
            insertStmt.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void delete(Integer ID) {
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement preparedStatement = connection
                    .prepareStatement("delete from drugs where id=?")) {
            preparedStatement.setInt(1, ID);
            preparedStatement.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void update(Drug elem) {
        validator.validate(elem);
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement updateStmt = connection
                    .prepareStatement("update from drugs set name=?, price=?, description=?" +
                            "where id=?")) {
            updateStmt.setString(1, elem.getName());
            updateStmt.setFloat(2, elem.getPrice());
            updateStmt.setString(3, elem.getDescription());
            updateStmt.setInt(4, elem.getID());
            updateStmt.executeUpdate();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public Drug find(Integer ID) {
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement findStmt = connection
                    .prepareStatement("select * from drugs where id=?")) {
            findStmt.setInt(1, ID);
            ResultSet resultSet = findStmt.executeQuery();
            if(resultSet.next() == false)
                return null;
            Integer id = resultSet.getInt("id");
            String name = resultSet.getString("name");
            Float price = resultSet.getFloat("price");
            String description = resultSet.getString("description");
            Drug drug = new Drug(id, name, price, description);
            return drug;
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public Collection<Drug> findAll() {
        Collection<Drug> drugs = new ArrayList<>();
        try(Connection connection = jdbcUtils.getConnection();
            PreparedStatement findStmt = connection
                    .prepareStatement("select * from drugs")) {
            ResultSet resultSet = findStmt.executeQuery();
            while(resultSet.next()) {
                Integer id = resultSet.getInt("id");
                String name = resultSet.getString("name");
                Float price = resultSet.getFloat("price");
                String description = resultSet.getString("description");
                Drug drug = new Drug(id, name, price, description);
                drugs.add(drug);
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return  drugs;
    }
}
