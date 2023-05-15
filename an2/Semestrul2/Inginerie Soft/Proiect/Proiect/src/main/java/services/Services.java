package services;

import domain.*;
import domain.validators.ValidationException;
import javafx.util.Pair;
import repository.IDrugRepository;
import repository.IOrderItemRepository;
import repository.IOrderRepository;
import repository.IUserRepository;
import utils.events.*;
import utils.observer.Observable;
import utils.observer.Observer;

import java.util.*;
import java.util.stream.Collectors;

public class Services implements Observable<Event> {

    private IUserRepository userRepository;
    private IDrugRepository drugRepository;
    private IOrderRepository orderRepository;
    private IOrderItemRepository orderItemRepository;
    private Map<Integer, Boolean> loggedUsers;

    public Services(IUserRepository userRepository, IDrugRepository drugRepository,
                    IOrderRepository orderRepository, IOrderItemRepository orderItemRepository) {
        this.userRepository = userRepository;
        this.drugRepository = drugRepository;
        this.orderRepository = orderRepository;
        this.orderItemRepository = orderItemRepository;
        loggedUsers = new HashMap<>();
    }

    public void addDrug(Drug drug){
        drugRepository.save(drug);
        notifyObservers(new MedsManagementEvent(MedsManagementType.ADD, drug));
    }

    public void updateDrug(Drug drug){
        Collection<OrderItem> orderItems = orderItemRepository.findAll();
        orderItems.forEach(orderItem -> {
            if(orderItem.getDrug().getID().equals(drug.getID())) {
                orderItem.setDrug(drug);
                orderItem.setDrugName(drug.getName());
                orderItemRepository.update(orderItem);
            }
        });
        drugRepository.update(drug);
        notifyObservers(new MedsManagementEvent(MedsManagementType.UPDATE, drug));
    }

    public void deleteDrug(Drug drug){
        Collection<OrderItem> orderItems = orderItemRepository.findAll();
        orderItems.forEach(orderItem -> {
            if(orderItem.getDrug().getID().equals(drug.getID())) {
                orderItemRepository.delete(orderItem.getID());
                orderRepository.delete(orderItem.getOrder().getID());
            }
        });
        drugRepository.delete(drug.getID());
        notifyObservers(new MedsManagementEvent(MedsManagementType.DELETE, drug));
    }


    public Collection<Drug> findAllDrugs(){
        Collection<Drug> drugs = drugRepository.findAll();
        return drugs;
    }

    public Collection< Pair<Order, Collection<OrderItem>> > getAllOrderForSection(User user){
        Collection<Order>  orders = orderRepository.findAll();
        Collection<OrderItem> orderItems = orderItemRepository.findAll();
        return  orders.stream()
                .filter(order -> user.getID().equals(order.getUser().getID()))
                .map(order -> {
                   Collection<OrderItem> filteredOrderItems = orderItems.stream()
                           .filter(orderItem -> order.getID().equals(orderItem.getOrder().getID()))
                           .collect(Collectors.toList());
                   return new Pair<Order, Collection<OrderItem>>(order, filteredOrderItems);
                }).collect(Collectors.toList());
    }

    public void addOrder(Order order, Collection<OrderItem> orderItems){
        orderRepository.save(order);
        for(OrderItem orderItem: orderItems){
            orderItem.setOrder(order);
            orderItem.setID(new Pair<>(orderItem.getDrug().getID(), orderItem.getOrder().getID()));
            orderItemRepository.save(orderItem);
        }
        notifyObservers(new OrderEvent(OrderEventType.ADD, order));
    }

    public Collection<Pair<Order, Collection<OrderItem>>> filterOrdersByStatus(Status status){
        Collection<Order> orders = orderRepository.findAll();
        Collection<OrderItem> orderItems = orderItemRepository.findAll();
        return orders.stream()
                .filter(order -> order.getStatus().equals(status))
                .map(order -> {
                    Collection<OrderItem> filteredOrderItems = orderItems.stream()
                            .filter(orderItem -> orderItem.getOrder().getID().equals(order.getID()))
                            .collect(Collectors.toList());
                    return new Pair<Order, Collection<OrderItem>>(order, filteredOrderItems);
                }).collect(Collectors.toList());
    }

    public Collection<Order> getOrdersByStatus(Status status){
        Collection<Order> orders = orderRepository.findAll()
                .stream()
                .filter(order -> order.getStatus().equals(status))
                .collect(Collectors.toList());
        return orders;
    }

    public void honorOrder(Order order){
        order.setStatus(Status.HONORED);
        orderRepository.update(order);
        notifyObservers(new OrderEvent(OrderEventType.HONOR, order));
    }

    public void refuseOrder(Order order){
        order.setStatus(Status.REFUSED);
        orderRepository.update(order);
        notifyObservers(new OrderEvent(OrderEventType.REFUSED, order));
    }

    public Collection<OrderItem> getAllOrderItemsForOrder(Order order){
        Collection<OrderItem> orderItems = orderItemRepository.findAll();
        return orderItems.stream()
                .filter(orderItem -> orderItem.getOrder().getID().equals(order.getID()))
                .collect(Collectors.toList());
    }

    public void signUp(User user){
        userRepository.save(user);
    }

    public void logout(User user){
        if(loggedUsers.containsKey(user.getID())){
            loggedUsers.remove(user.getID());
            return;
        }
        throw new ValidationException("User " + user.getUsername() + " is not login !");
    }

    public User login(User user){
        User findUserSystem = userRepository.filterByUsernameAndPassword(user);
        if(findUserSystem == null)
            throw new ValidationException("User " + user.getUsername() + " is not register in system !");
        if(loggedUsers.containsKey(findUserSystem.getID()))
            throw new ValidationException("User " + user.getUsername() + " is already login !");
        loggedUsers.put(findUserSystem.getID(), true);
        return findUserSystem;
    }


    private List<Observer<Event>> observersMedsManagementEvent = new ArrayList<>();

    @Override
    public void addObserver(Observer<Event> observer) {
        observersMedsManagementEvent.add(observer);
    }

    @Override
    public void removeObserver(Observer<Event> observer) {
        observersMedsManagementEvent.remove(observer);
    }

    @Override
    public void notifyObservers(Event event) {
        observersMedsManagementEvent.stream().forEach(x -> x.update(event));
    }
}
