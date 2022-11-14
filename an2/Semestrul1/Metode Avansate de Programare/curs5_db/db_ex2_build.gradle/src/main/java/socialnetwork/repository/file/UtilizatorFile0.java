package socialnetwork.repository.file;

import socialnetwork.domain.Utilizator;
import socialnetwork.domain.validators.Validator;

import java.util.List;

public class UtilizatorFile0 extends AbstractFileRepository0<Long, Utilizator> {

    public UtilizatorFile0(String fileName, Validator<Utilizator> validator) {
        super(fileName, validator);
    }

    @Override
    public Utilizator extractEntity(List<String> attributes) {
        //TODO: implement method
        Utilizator user = new Utilizator(attributes.get(1),attributes.get(2));
        user.setId(Long.parseLong(attributes.get(0)));

        return user;
    }

    @Override
    protected String createEntityAsString(Utilizator entity) {
        return entity.getId()+";"+entity.getFirstName()+";"+entity.getLastName();
    }
}
