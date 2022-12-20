using Sem11_12.Repository;
using Sem11_12.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sem11_12.Service
{
    class SarcinaService
    {
        private IRepository<string, Sarcina> repo;

        public SarcinaService(IRepository<string, Sarcina> repo)
        {
            this.repo = repo;
        }



        public List<Sarcina> FindAllSarcini()
        {
            return repo.FindAll().ToList();
        }
    }
}
