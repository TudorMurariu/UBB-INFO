package repository;

import domain.User;

public interface IUserRepository extends Repository<User, Integer> {
    User filterByUsernameAndPassword(User user);
}
