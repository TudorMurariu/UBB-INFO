using Sem11_12.Model;
using Sem11_12.Model.Validator;

using System;
using System.Collections.Generic;
using System.Linq;


namespace Sem11_12.Repository
{
    class InMemoryRepository<ID, E> : IRepository<ID, E> where E : Entity<ID>
    {
        protected IValidator<E> vali;

        protected IDictionary<ID, E> entities=new Dictionary<ID, E>();

        public InMemoryRepository(IValidator<E> vali)
        {
            this.vali = vali;
        }

        public E Delete(ID id)
        {
            throw new NotImplementedException();
        }

        public IEnumerable<E> FindAll()
        {
            return entities.Values.ToList<E>();
        }

        public E FindOne(ID id)
        {
            throw new NotImplementedException();
        }

        public E Save(E entity)
        {
            if (entity == null)
                throw new ArgumentNullException("entity must not be null");
            this.vali.Validate(entity);
            if(this.entities.ContainsKey(entity.ID)){
                return entity;
            }
            this.entities[entity.ID] = entity;
            return default(E);
        }

        public E Update(E entity)
        {
            throw new NotImplementedException();
        }
    }
}
