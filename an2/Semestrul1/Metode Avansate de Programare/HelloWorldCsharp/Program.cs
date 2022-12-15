using System;
using System.Collections.Generic;

namespace MyApp // Note: actual namespace depends on the project name.
{
    internal class Program
    {
        static void Main(string[] args)
        {
            //Console.Write("What's your name? ");
            //string? name = Console.ReadLine();
            //Console.WriteLine("Hello " + name);

            int[] v = new int[] { 1, 2, 3, 4, 2, 7, 9 };

            Array.Sort(v);
            Console.WriteLine(v);

            Console.WriteLine("\n");

            List<int> values = new List<int>(v);
            Dictionary<int, int> dictionary = new Dictionary<int, int>();
            foreach (var x in values)
            {
                if (dictionary.ContainsKey(x))
                    dictionary[x]++;
                else 
                    dictionary[x] = 1;
                Console.Write(x + " ");
            }

            Console.WriteLine("\n");

            foreach (var key in dictionary.Keys)
                Console.WriteLine(key + " : " + dictionary[key]);
            
        }
    }
}