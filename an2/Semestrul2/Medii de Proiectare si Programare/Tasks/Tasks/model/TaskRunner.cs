using System;
using tasks.model;
using tasks.utils;
using System.Collections.Generic;
namespace tasks.runner
{
	public interface ISubject<T> where T:EventArgs
	{
		event EventHandler<T> ObservableEvent;
	}
	public interface ITaskRunner:/* ISubject<TaskExecutionEvent>*/ IObservable<TaskExecutionEvent>
	{
		//event EventHandler<TaskExecutionEvent> ObservableEvent;
		void Add(Task t);
		void executeOne();
		void executeAll();
	}

	public class QueueTaskRunner : ITaskRunner
	{
		//public event EventHandler<TaskExecutionEvent> ObservableEvent;
		IContainer con;
		public QueueTaskRunner()
		{
			con = new Container();
		}
		public void Add(Task t)
		{
			con.Add(t);
		}

		public void executeAll()
		{
			while(con.Length>0)
			{
				executeOne();
			}
				
		}

		public void executeOne()
		{
			if (con.Length > 0)
			{
				//OnTaskExecution(new TaskExecutionEvent(ExecutionEvent.Starting, con[0]));
				notifyObservers(new TaskExecutionEvent(ExecutionEvent.Starting, con[0]));
				con[0].execute();
				//	OnTaskExecution(new TaskExecutionEvent(ExecutionEvent.Finished, con[0]));
				notifyObservers(new TaskExecutionEvent(ExecutionEvent.Finished, con[0]));
				con.delete(0);
			}
			else {
				throw new RunnerException("Container is empty!");
			}
		}

		/*protected virtual void OnTaskExecution(TaskExecutionEvent ev)
		{
			if (ObservableEvent != null)
				ObservableEvent(this, ev);
		}*/

		private void notifyObservers(TaskExecutionEvent e)
		{
			foreach (IObserver<TaskExecutionEvent> obs in observers)
				obs.OnNext(e);
		}

		private List<IObserver<TaskExecutionEvent>> observers=new List<IObserver<TaskExecutionEvent>>();


		public IDisposable Subscribe(IObserver<TaskExecutionEvent> observer)
		{
			if (!observers.Contains(observer))
				observers.Add(observer);
			return new Unsubscriber(observers, observer);
		}

		private class Unsubscriber : IDisposable
		{
			private List<IObserver<TaskExecutionEvent>> _observers;
			private IObserver<TaskExecutionEvent> _observer;

			public Unsubscriber(List<IObserver<TaskExecutionEvent>> observers, IObserver<TaskExecutionEvent> observer)
			{
				this._observers = observers;
				this._observer = observer;
			}

			public void Dispose()
			{
				if (_observer != null && _observers.Contains(_observer))
					_observers.Remove(_observer);
			}
		}


	}

	public enum ExecutionEvent
	{
		Starting, Cancelled, Finished
	}
	public class TaskExecutionEvent : EventArgs 
	{
		readonly ExecutionEvent taskEvent;
		readonly Task data;

		public TaskExecutionEvent(ExecutionEvent eventT, Task data)
		{
			this.taskEvent = eventT;
			this.data = data;
		}
		public ExecutionEvent Event { 
			get { return taskEvent;}
		}

		public Task Data
		{
			get { return data;}
		}
	}
}
