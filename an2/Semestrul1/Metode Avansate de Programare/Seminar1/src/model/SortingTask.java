package model;

import sortStuff.AbstractSorter;
import sortStuff.SortingBubble;
import sortStuff.SortingQuick;
import sortStuff.SortingStrategy;

public class SortingTask extends Task {

    public int[] array = new int[0];
    private AbstractSorter taskSortter;

    public SortingTask(String taskId, String description, int[] arr, SortingStrategy strategy) {
        super(taskId, description);
        array = arr;

        if(strategy == SortingStrategy.QUICK)
            this.taskSortter = new SortingQuick();
        else
            this.taskSortter = new SortingBubble();
    }

    public void sort() {
        //Random r = new Random();

        // Arrays.sort(array);

        /*if(r.nextInt() % 2 == 1) {
            taskSortter = new SortingBubble();
            taskSortter.sort(array);
        }
        else {
            taskSortter = new SortingQuick();
            taskSortter.sort(array);
        }*/

        taskSortter.sort(array);
    }
    @Override
    public void execute() {
        sort();
        System.out.println("The array is:");
        for(int x : array)
            System.out.println(x);
    }
}
