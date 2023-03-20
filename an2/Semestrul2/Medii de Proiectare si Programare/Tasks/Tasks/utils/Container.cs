using System;
using tasks.model;
namespace tasks.utils
{
	public class Container:IContainer
	{
		private Task[] elems;
		private int len, cap;
		public Container()
		{
			cap = 2;
			len = 0;
			elems = new Task[cap];
		}
		public void Add(Task e)
		{
			if (len == cap)
				resize();
			elems[len++] = e;
		}

		private void resize()
		{
			cap *= 2;
			Task[] tmp = new Task[cap];
			for (int i = 0; i < elems.Length; i++)
				tmp[i] = elems[i];
			elems = tmp;
		}

		public int Length
		{
			get { return len;}
		}

		public Task this [int index]
		{
			get
			{
				if ((index >= 0)&& (index < len))
					return elems[index];
				return null;
			}

			set 
			{
				if ((index >= 0) && (index < len))
					elems[index] = value;
			}
		}

		public Task delete(int index)
		{
			if ((index >= 0) && (index < len)){
				Task e = elems[index];
				for (int i = index; i < len - 1; i++)
					elems[i] = elems[i + 1];
				len--;
				return e;
			}
			return null;
		}

		public bool delete(Task t)
		{
			for (int i = 0; i < len; i++)
			{
				if (elems[i] == t)
				{
					for (int j = i; j < len - 1; j++)
						elems[j] = elems[j + 1];
					return true;
				}
					
			}
			return false;
		}


	}
}
