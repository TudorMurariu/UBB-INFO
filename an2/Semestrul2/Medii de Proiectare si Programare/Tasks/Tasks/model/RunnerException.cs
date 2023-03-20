using System;
namespace tasks.runner
{
	public class RunnerException:ApplicationException
	{
		public RunnerException(){}
		public RunnerException(String ms) : base(ms){}
		public RunnerException(String ms, Exception e) : base(ms, e) { }
	}
}
