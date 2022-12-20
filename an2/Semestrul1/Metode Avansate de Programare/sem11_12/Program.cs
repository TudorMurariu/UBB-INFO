
using Sem11_12.Model;
using Sem11_12.Model.Validator;
using Sem11_12.Repository;
using Sem11_12.Service;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Security.Cryptography;

namespace Sem11_12
{
    class Program
    {

        static void Main(string[] args)
        {


            List<Angajat> angajati = GetAngajatService().FindAllAngajati();

            List<int> l = new List<int>() { 1,2,3,4,5,6,7,8,9,10};          
            var res=l.Where(x => x % 2 == 0);
       
            res.ToList().ForEach(Console.WriteLine);

            DateTime date = DateTime.Now;
            Console.WriteLine(date.ToString("dd/mm/yy hh:MM"));




            // 1. afisati doar angajatii care au nivelul junior - extension methods "Where"

            List<Angajat> juniors = 
                angajati.Where(angajat => angajat.Nivel == KnowledgeLevel.Junior)
                .ToList();

            //2  - cerinta 1 din pdf sem11-12  extension methods 

            List<Angajat> orderd = angajati.OrderBy(angajat => angajat.Nivel)
                .ThenByDescending(angajat => angajat.VenitPeOra)
                .ToList();

            //2  - cerinta 1 din pdf sem11-12  sql like 

            var orderedSql = from angajat in angajati
                                       orderby angajat.Nivel
                                       orderby angajat.VenitPeOra descending
                                       select angajat;


            //2 cate ore dureaza in medie fiecare tip de sarcina

            /*List<Sarcina> sarcini = GetSarcinaService().FindAllSarcini();
            sarcini.GroupBy(s => s.TipDificultate)
                .Select(v => Touple.Create(v.Key, v.Average)*/

            //3 primii doi cei mai harnici angajati
            //List<Angajat> firstTwo = angajati.OrderBy(a => a.);

        }

        private static void Task2()
        {
            

        }


        private static AngajatService GetAngajatService()
        {
            //string fileName2 = ConfigurationManager.AppSettings["angajatiFileName"];
            string fileName = "..\\..\\..\\data\\angajati.txt";
            IValidator<Angajat> vali = new AngajatValidator();

            IRepository<string, Angajat> repo1 = new AngajatInFileRepository(vali, fileName);
            AngajatService service = new AngajatService(repo1);
            return service;
        }

        private static SarcinaService GetSarcinaService()
        {
            //string fileName2 = ConfigurationManager.AppSettings["sarciniFileName"];
            string fileName2 = "..\\..\\..\\data\\sarcini.txt";
            IValidator<Sarcina> vali = new SarcinaValidator();

            IRepository<string, Sarcina> repo1 = new SarcinaInFileRepository(vali, fileName2);
            SarcinaService service = new SarcinaService(repo1);
            return service;
        }

        private static PontajService GetPontajService()
        {
            //string fileName2 = ConfigurationManager.AppSettings["pontajeFileName"];
            string fileName2 = "..\\..\\..\\data\\pontaje.txt";
            IValidator<Pontaj> vali = new PontajValidator();

            IRepository<string, Pontaj> repo1 = new PontajInFileRepository(vali, fileName2);
            PontajService service = new PontajService(repo1);
            return service;
        }

    }
}
