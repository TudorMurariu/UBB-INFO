using System;
using System.Xml.Serialization;
namespace tasks.model
{
	public enum SortingAlgorithm
	{
		BUBBLE_SORT, QUICK_SORT, SELECTION_SORT
	}

	public enum SortingOrder
	{
		Ascending, Descending
	}

	public class SortingTask:Task
	{
		private int elems;
		private SortingOrder order;
		private SortingAlgorithm alg;
		public SortingTask():base(0,"")
		{
		}
		public SortingTask(int id, String desc, int elems,SortingOrder ord, SortingAlgorithm alg):base(id,desc)
		{
			this.elems = elems;
			this.order = ord;
			this.alg = alg;
		}

		public override void execute()
		{
			Console.WriteLine("Executing sorting task {0}", Id);
		}
		[XmlAttribute]
		public SortingOrder Order
		{
			get { return order;}
			set { order = value; }
		}

		public SortingAlgorithm Algorithm
		{
			get { return alg;}
			set { alg = value; }
		}

		public int Elems
		{
			get { return elems; }
			set { elems = value; }
		}
		public override string ToString()
		{
			return string.Format("[Id={0}, Desc={1}, Elems={4}, Order={2}, Algo={3}]", Id, Description, Order, Algorithm,Elems);
		}


	}
}

