using Sem11_12.Model;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sem11_12.Model
{
    class Pontaj : Entity<string>
    {
        public Angajat Angajat { get; set; }
        public Sarcina Sarcina { get; set; }

        public DateTime Date { get; set; }

        public override string ToString()
        {

            //return Angajat.ToString() + " " + Sarcina.ToString() +" "+Date.ToString("d/M/yyyy", System.Globalization.CultureInfo.InvariantCulture);
           // return String.Format("{0} {1} {2}", Angajat, Sarcina, Date.ToString("d/M/yyyy", System.Globalization.CultureInfo.InvariantCulture));
            return $"{Angajat} {Sarcina} {Date.ToString("d/M/yyyy", CultureInfo.InvariantCulture)}"; //C#6
        }
    }
}
