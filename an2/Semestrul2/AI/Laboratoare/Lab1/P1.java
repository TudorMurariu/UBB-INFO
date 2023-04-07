import java.util.Arrays;

public class P1{
    /*
     * Să se determine ultimul (din punct de vedere alfabetic) 
     * cuvânt care poate apărea într-un text care conține mai multe cuvinte 
     * separate prin ” ” (spațiu). De ex. ultimul (dpdv alfabetic) cuvânt din 
     * ”Ana are mere rosii si galbene” este cuvântul "si".
     */
    public static void main(String[] args) {
        test();
    }

    // Time complexity:  O(n * strcmp(s))
    // where s is the longest word in the phrase
    // Space complexity: O(n)
    // we use an array
    private static String getLastFast(String inputString) {
        String[] words = inputString.split(" ");
        String last = words[0];
        for(String s : words) 
            if(last.compareTo(s) < 0)
                last = s;
        return last;
    }

    // Time complexity:  O(n * strcmp(s))
    // where s is the longest word in the phrase
    // Space complexity: O(n)
    // we use an array
    private static String getLastSlow(String inputString) {
        String[] words = inputString.split(" ");
        Arrays.sort(words);  
        return words[words.length];
    }

    private static void test() {
        String inputString1 = "Ana are mere rosii si galbene";
        String inputString2 = "A B C D E 1 2 3 4";
        String inputString3 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit";
        String inputString4 = "Phasellus rhoncus erat sapien, et sodales urna mollis nec.";
        
        assert(getLastFast(inputString1).equals("si"));
        assert(getLastFast(inputString2).equals("E"));
        assert(getLastFast(inputString3).equals("sit"));
        assert(getLastFast(inputString4).equals("urna"));
    }
}