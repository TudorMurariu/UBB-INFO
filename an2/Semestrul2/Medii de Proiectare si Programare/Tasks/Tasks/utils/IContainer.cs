using System;
using tasks.model;
namespace tasks.utils
{
	public interface IContainer
	{
		void Add(Task e);
		int Length { get; }
		Task this[int indexer] { get; set; }
		Task delete(int index);
		bool delete(Task t);
	}
}
