using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sem11_12.Model
{
    abstract class Task : Entity<string>
    {
        private string description;

        public Task()
        {
        }


        public string ID { get; set ; }
        public string Description { get => description; set => description = value; }

        public abstract void Execute();

        public override string ToString()
        {
            return "id=" + ID + "|description=" + Description;
        }
    }
}
