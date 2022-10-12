package model;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Random;

public class SortingTask extends Task {

    public int[] array = new int[0];

    public SortingTask(String taskId, String description, int[] arr) {
        super(taskId, description);
        array = arr;
    }

    public void sort() {
        Random r = new Random();
        if(r.nextInt() % 2 == 1)
            Arrays.sort(array);
        else {
            boolean ok = true;
            while(ok)
            {
                ok = false;
                for(int i=0;i<array.length-1;++i)
                    if(array[i] > array[i+1])
                    {
                        ok = true;
                        int aux = array[i];
                        array[i] = array[i+1];
                        array[i+1] = aux;
                    }
            }
        }
    }
    @Override
    public void execute() {
        sort();
        System.out.println("The array is:");
        for(int x : array)
            System.out.println(x);
    }
}
