using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sem11_12.Model
{
    class MessageTask : Task
    {
        private string message;
        private string from;
        private string to;
        private DateTime date;
        

        public string Message { get => message; set => message = value; }
        public string From { get => from; set => from = value; }
        public string To { get => to; set => to = value; }
        public DateTime Date { get => date; set => date = value; }

        public override void Execute()
        {
            Console.WriteLine("Mesaj!");
        }

        public override string ToString()
        {
            return base.ToString() + "mesaj=" + message
                + "from=" + from + "to=" + to
                + "date=" + date;
        }
    }
}
