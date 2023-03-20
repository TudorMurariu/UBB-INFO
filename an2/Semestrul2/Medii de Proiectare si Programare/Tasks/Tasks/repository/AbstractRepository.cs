using System;
using System.Collections.Generic;
using tasks.model;
namespace tasks.repository
{
	public abstract class AbstractRepository<ID, T> : ICrudRepository<ID, T> where T : HasId<ID>
	{
		
		IDictionary<ID, T> items;

		public AbstractRepository(IValidator<T> validator)
		{
			items = new Dictionary<ID, T>();

		}

		public AbstractRepository()
		{
			items = new Dictionary<ID, T>();
		}


		public virtual void delete(ID id)
		{
			items.Remove(id);

		}

		public IEnumerable<T> findAll()
		{
			return items.Values;
		}

		public T findOne(ID id)
		{
			
			if (items.ContainsKey(id))
				return items[id];
			else
				return default(T);

		}

		public virtual void save(T entity)
		{
			if (!items.ContainsKey(entity.Id))
				items.Add(entity.Id, entity);
			else throw new RepositoryException("Duplicate entity " + entity);
		}
	}

}
