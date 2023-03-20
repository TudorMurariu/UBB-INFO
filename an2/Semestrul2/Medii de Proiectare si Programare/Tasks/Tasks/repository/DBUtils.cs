using System;
using System.Data;
using System.Collections.Generic;

namespace tasks.repository
{
	public static class DBUtils
	{
		

		private static IDbConnection instance = null;


		public static IDbConnection getConnection(IDictionary<string,string> props)
		{
			if (instance == null || instance.State == System.Data.ConnectionState.Closed)
			{
				instance = getNewConnection(props);
				instance.Open();
			}
			return instance;
		}

		private static IDbConnection getNewConnection(IDictionary<string,string> props)
		{
			
			return ConnectionUtils.ConnectionFactory.getInstance().createConnection(props);


		}
	}
}
