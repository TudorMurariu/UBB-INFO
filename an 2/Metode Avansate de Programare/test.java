import java.util.*;

public class test{
    public static void main(String[] args)
    {
        String a = "";
        System.out.println(solution(a));
        System.out.println(a.length());
    }


    public static int solution(String S) {
        // O(n) - time complexity
        // O(n) - space complexity
        HashSet<Character> set = new HashSet<>();

        int count = 1;
        for(int i=0;i<S.length();++i)
            if(set.contains(S.charAt(i)))
            {
                ++count;
                set.clear();
                set.add(S.charAt(i));
            }
            else
             set.add(S.charAt(i));

        return count;
    }
}