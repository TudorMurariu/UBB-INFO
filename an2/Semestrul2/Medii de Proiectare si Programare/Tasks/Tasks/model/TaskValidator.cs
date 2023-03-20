using System;
using System.Text;
namespace tasks.model
{
	public class TaskValidator : IValidator<Task>
	{
		public void validate(Task elem)
		{
			StringBuilder errorString = new StringBuilder();

			if (elem.Description == "")
				errorString.Append("The name must not be void");
			if (elem.Id < 0)
				errorString.Append("The id must be a nonnegative number");

			if (errorString.Length != 0)
				throw new ValidationException(errorString.ToString());
		}
	}
}
