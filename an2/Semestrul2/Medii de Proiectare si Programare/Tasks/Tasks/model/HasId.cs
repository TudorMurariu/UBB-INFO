using System;
namespace tasks.model
{
	public interface HasId<T>
	{
		T Id { get; set; }
	}
}
