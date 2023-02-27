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
    private static String getLast(String inputString) {
        String[] words = inputString.split(" ");
        String last = words[0];
        for(String s : words) 
            if(last.compareTo(s) < 0)
                last = s;
        return last;
    }

    private static void test() {
        String inputString1 = "Ana are mere rosii si galbene";
        String inputString2 = "A B C D E 1 2 3 4";
        String inputString3 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit";
        String inputString4 = "Phasellus rhoncus erat sapien, et sodales urna mollis nec.";
        
        assert(getLast(inputString1).equals("si"));
        assert(getLast(inputString2).equals("E"));
        assert(getLast(inputString3).equals("sit"));
        assert(getLast(inputString4).equals("urna"));
    }
}