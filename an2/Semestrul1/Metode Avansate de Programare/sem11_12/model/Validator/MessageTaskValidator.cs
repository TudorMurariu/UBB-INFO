using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Sem11_12.Model;
namespace Sem11_12.Model.Validator
{
    class MessageTaskValidator : IValidator<MessageTask>
    {
        public void Validate(MessageTask e)
        {
            bool valid = true;
            if (valid == false)
            {
                throw new ValidationException("Obiectul nu e valid");
            }
        }
    }
}
