using Sem11_12.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sem11_12.Model
{
    enum Dificultate { Usoara, Medie, Grea }
    class Sarcina : Entity<string>
    {
        public Dificultate TipDificultate { get; set; }
        public int NrOreEstimate { get; set; }
        public override string ToString()
        {
            return ID + " " + TipDificultate + " " + NrOreEstimate;
        }
    }

}
