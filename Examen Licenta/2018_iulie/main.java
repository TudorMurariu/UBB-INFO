import java.util.*;
// import javafx.util.Pair;

class Pair<K, V> {
    private K key;
    private V value;

    public Pair(K key, V value) {
        this.key = key;
        this.value = value;
    }

    public K getKey() {
        return key;
    }

    public V getValue() {
        return value;
    }
    
    public void setKey(K key) {
        this.key = key;
    }

    public void setValue(V value) {
        this.value = value;
    }
}



abstract class Vehicle {
    private int basePrice;
    
    public Vehicle(int basePrice) throws Exception
    {
        if(basePrice <= 0)
            throw new Exception("basePrice trebuie sa fie strict pozitiv");
        this.basePrice = basePrice;
    }
    
    public abstract String description();
    
    public int getPrice()
    {
        return this.basePrice;
    }
}

class Car extends Vehicle {
    private String model;
    
    public Car(int basePrice, String model) throws Exception
    {
        super(basePrice);
        if(model == null || model.isEmpty())
            throw new Exception("model trebuie sa nu fie null sau empty");
        this.model = model;
    }
    
    public String description() 
    {
        return model;
    }
}

class AutomaticCar extends Car {
    private int additionalPrice;
    
    public AutomaticCar(int basePrice, String model, int additionalPrice) throws Exception
    {
        super(basePrice, model);
        if(additionalPrice <= 0)
            throw new Exception("additionalPrice trebuie sa fie strict pozitiv");
        this.additionalPrice = additionalPrice;
    }
    
    @Override public String description() 
    {
        return "Automatic Car " + super.description();
    }
    
    @Override public int getPrice()
    {
        return super.getPrice() + this.additionalPrice;
    }
}

class CarWithcparkingSensor extends Car {
    private String sensorType;
    
    public CarWithcparkingSensor(int basePrice, String model, String sensorType) throws Exception
    {
        super(basePrice, model);
        if(sensorType == null || sensorType.isEmpty())
            throw new Exception("sensorType trebuie sa nu fie null sau empty");
        this.sensorType = sensorType;
    }
    
    @Override public String description() 
    {
        return "Car with parking sensor " + this.sensorType + " " + super.description();
    }
    
    @Override public int getPrice()
    {
        return super.getPrice() + 2500;
    }
}

public class Main
{
	public static void main(String[] args) throws Exception {
	    Car a = new Car(3, "Audi");
	    Car b = new CarWithcparkingSensor(3, "Audi", "1");
        Car c = new AutomaticCar(3, "Audi", 1000);
        Car d = new AutomaticCar(100, "Mercedes", 1000);
        Car e = new AutomaticCar(2, "Toyota", 1000);
	    
	    ArrayList<Vehicle> l = new ArrayList<>();
	    l.add(a);
	    l.add(b);
	    l.add(c);
        l.add(d);
        l.add(e);
        l.add(e);
        l.add(e);
	    
	    printModels(l);
	    
		System.out.println("Hello World" + a.getPrice());
		System.out.println("Hello World" + b.getPrice());
		
		arrange(l);
		System.out.println(l);
		printModels(l);
		
		
		List<Pair<String, Integer>> newL = countByModel(l);
		for (Pair<String, Integer> p : newL)
		{
		    System.out.println(p.getKey() + " " + p.getValue());
		}
	}
	
	
	static List<Pair<String, Integer>> countByModel(List<Vehicle> list)
    {
        List<Pair<String, Integer>> modelList = new ArrayList<>(); 
        for (Vehicle v : list)
        {
            if(v instanceof Car)
            {
                Car c = (Car) v;
                boolean found = false;
                for(Pair<String, Integer> p : modelList)
                    if(p.getKey().equals(c.description()))
                    {
                        p.setValue(p.getValue() + 1);
                        found = true;
                        break;
                    }
                    
                if(!found)
                    modelList.add(new Pair<>(c.description(), 1));
            }
        }
        
        return modelList;
    }

    static void arrange(List<Vehicle> list) {
        int j = 0;
        for(int i = 0; i < list.size(); ++i) {
            int price = list.get(i).getPrice();
            if(price >= 1000 && price <= 2000) {
                Vehicle aux = list.get(i);
                list.set(i, list.get(j));
                list.set(j, aux);
                ++j;
            }
        }
    }
    
    static void printModels(List<Vehicle> list) {
        for (Vehicle v : list)
            if(v instanceof Car)
            {
                Car c = (Car) v;
                System.out.println("   " + c.description());
            }
    }
}