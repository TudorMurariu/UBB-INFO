using System;
using tasks.model;
using System.Collections.Generic;
using System.Data;
using log4net;
namespace tasks.repository
{
	public class SortingTaskDbRepository: ICrudRepository<int,SortingTask>
	{
		private static readonly ILog log = LogManager.GetLogger("SortingTaskDbRepository");

		IDictionary<String, string> props;
		public SortingTaskDbRepository(IDictionary<String, string> props)
		{
			log.Info("Creating SortingTaskDbRepository ");
			this.props = props;
		}

		public SortingTask findOne(int id)
		{
			log.InfoFormat("Entering findOne with value {0}", id);
			IDbConnection con = DBUtils.getConnection(props);

			using (var comm = con.CreateCommand())
			{
				comm.CommandText = "select id,descriere, Elems, orderC, algoritm from SortingTasks where id=@id";
				IDbDataParameter paramId = comm.CreateParameter();
				paramId.ParameterName = "@id";
				paramId.Value = id;
				comm.Parameters.Add(paramId);

				using (var dataR = comm.ExecuteReader())
				{
					if (dataR.Read())
					{
						int idV = dataR.GetInt32(0);
						String desc = dataR.GetString(1);
						int elems = dataR.GetInt32(2);
						SortingOrder order = (SortingOrder)Enum.Parse(typeof(SortingOrder), dataR.GetString(3));
						SortingAlgorithm algo = (SortingAlgorithm)Enum.Parse(typeof(SortingAlgorithm), dataR.GetString(4));
						SortingTask task = new SortingTask(idV, desc, elems, order, algo);
						log.InfoFormat("Exiting findOne with value {0}", task);
						return task;
					}
				}
			}
			log.InfoFormat("Exiting findOne with value {0}", null);
			return null;
		}

		public IEnumerable<SortingTask> findAll()
		{
			IDbConnection con = DBUtils.getConnection(props);
			IList<SortingTask> tasksR = new List<SortingTask>();
			using (var comm = con.CreateCommand())
			{
				comm.CommandText = "select id,descriere, Elems, orderC, algoritm from SortingTasks";

				using (var dataR = comm.ExecuteReader())
				{
					while (dataR.Read())
					{
						int idV = dataR.GetInt32(0);
						String desc = dataR.GetString(1);
						int elems = dataR.GetInt32(2);
						SortingOrder order = (SortingOrder)Enum.Parse(typeof(SortingOrder), dataR.GetString(3));
						SortingAlgorithm algo = (SortingAlgorithm)Enum.Parse(typeof(SortingAlgorithm), dataR.GetString(4));
						SortingTask task = new SortingTask(idV, desc, elems, order, algo);
						tasksR.Add(task);
					}
				}
			}

			return tasksR;
		}
		public void save(SortingTask entity)
		{
			var con = DBUtils.getConnection(props);

			using (var comm = con.CreateCommand())
			{
				comm.CommandText = "insert into SortingTasks  values (@idT, @desc, @elems, @orderC, @algo)";
				var paramId = comm.CreateParameter();
				paramId.ParameterName = "@idT";
				paramId.Value = entity.Id;
				comm.Parameters.Add(paramId);

				var paramDesc = comm.CreateParameter();
				paramDesc.ParameterName = "@desc";
				paramDesc.Value = entity.Description;
				comm.Parameters.Add(paramDesc);

				var paramElems = comm.CreateParameter();
				paramElems.ParameterName = "@elems";
				paramElems.Value = entity.Elems;
				comm.Parameters.Add(paramElems);

				IDbDataParameter paramOrder = comm.CreateParameter();
				paramOrder.ParameterName = "@orderC";
				paramOrder.Value = entity.Order.ToString();
				comm.Parameters.Add(paramOrder);

				IDbDataParameter paramAlgo = comm.CreateParameter();
				paramAlgo.ParameterName = "@algo";
				paramAlgo.Value = entity.Algorithm.ToString();
				comm.Parameters.Add(paramAlgo);

				var result = comm.ExecuteNonQuery();
				if (result == 0)
					throw new RepositoryException("No task added !");
			}
			
		}
		public void delete(int id)
		{
			IDbConnection con = DBUtils.getConnection(props);
			using (var comm = con.CreateCommand())
			{
				comm.CommandText = "delete from SortingTasks where id=@id";
				IDbDataParameter paramId = comm.CreateParameter();
				paramId.ParameterName = "@id";
				paramId.Value = id;
				comm.Parameters.Add(paramId);
				var dataR = comm.ExecuteNonQuery();
				if (dataR == 0)
					throw new RepositoryException("No task deleted!");
			}
		}

	}
}
