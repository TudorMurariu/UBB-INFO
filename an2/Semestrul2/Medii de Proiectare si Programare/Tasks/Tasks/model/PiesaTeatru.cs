using System;
namespace tasks.model
{
	public class PiesaTeatru : HasId<int>
	{
		public string Autori { get; set; }
		public string Titlu { get; set; }
		public int Id { get; set; }
		public PiesaTeatru()
		{
		}

		public PiesaTeatru(int id, String tit, String aut)
		{
			Id = id;
			Titlu = tit;
			Autori = aut;
		}

		public override string ToString()
		{
			return string.Format("[PiesaTeatru: Autori={0}, Titlu={1}, Id={2}]", Autori, Titlu, Id);
		}

		public override bool Equals(object obj)
		{
			PiesaTeatru pt = obj as PiesaTeatru;
			if (obj == null)
				return false;
			return Id== pt.Id;
		}

		public override int GetHashCode()
		{
			return base.GetHashCode();
		}
	}
}
