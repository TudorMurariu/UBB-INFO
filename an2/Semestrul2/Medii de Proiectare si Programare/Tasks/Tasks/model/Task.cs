using System;
using System.Xml;
using System.Xml.Serialization;
namespace tasks.model
{
	public class Task:HasId<int>
	{
		private int id;
		private String description;
		public Task(int id, string desc)
		{
			this.id = id;
			this.description = desc;
		}
		[XmlAttribute]
		public int Id
		{
			get { return id; }
			set { id = value; }
		}

		public string Description { 
			get { return description; } 
			set { description = value; } 
		}

		public override string ToString()
		{
			return string.Format("[Task: Id={0}, Description={1}]", Id, Description);
		}

		public virtual void execute()
		{
			Console.WriteLine("Executing task {0}", id);
		}

		public override bool Equals(object obj)
		{
			if (obj is Task)
			{
				Task st = obj as Task;
				return Id == st.Id;
			}
			return false;
		}

		public override int GetHashCode()
		{
			return base.GetHashCode();
		}

		public static bool operator==(Task t1, Task t2)
		{
			return t1.Id == t2.Id;
		}

		public static bool operator !=(Task t1, Task t2)
		{
			return t1.Id != t2.Id;
		}
	}
}
