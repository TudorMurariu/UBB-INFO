using Sem11_12.Model;
using Sem11_12.Model.Validator;
using Sem11_12.Repository;
using Sem11_12.Model;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sem11_12.Repository
{
    class AngajatInFileRepository : InFileRepository<string, Angajat>
    {

        public AngajatInFileRepository(IValidator<Angajat> vali, string fileName) : base(vali, fileName, EntityToFileMapping.CreateAngajat)           
        {
           
        }

    }
}
