using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sem11_12.Model
{
    enum KnowledgeLevel
    {
        Junior=1, Medium, Senior
    }
    class Angajat : Entity<string> , ICloneable
    {
        public String Nume { get; set; }
        public double VenitPeOra { get; set; }
        public KnowledgeLevel Nivel { get; set; }

        public object Clone()
        {
            throw new NotImplementedException();
        }

        public override string ToString()
        {
            return ID+" "+Nume+" "+VenitPeOra+" "+Nivel;
        }
    }
}
