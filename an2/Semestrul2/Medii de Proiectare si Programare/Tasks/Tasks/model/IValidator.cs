using System;
namespace tasks.model
{
	public interface IValidator<T>
	{
		void validate(T elem);
	}

	public class ValidationException : ApplicationException
	{
		public ValidationException(string message)
		: base(message) { }
	}
}
