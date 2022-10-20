package sortStuff;

public class SortingBubble implements AbstractSorter {

    @Override
    public void sort(int[] array) {
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
