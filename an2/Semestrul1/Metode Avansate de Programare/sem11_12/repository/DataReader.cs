using Sem11_12.Repository;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sem11_12.Repository
{
    class DataReader
    {
        public static List<T> ReadData<T>(string fileName, CreateEntity<T> createEntity)
        {
            List<T> list = new List<T>();
            using (StreamReader sr = new StreamReader(fileName))
            {
                string s;
                while ((s = sr.ReadLine()) != null)
                {
                    T entity= createEntity(s);
                    list.Add(entity);
                }
            }
            return list;
        }
    }
}

