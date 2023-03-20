using System;
using tasks.model;
using System.IO;
namespace tasks.repository
{
	public delegate T ConvertFromString<T>(String line);
	public delegate String ConvertToString<T>(T t);
	public class FileRepository<ID, T> :AbstractRepository<ID,T> where T:HasId<ID>
	{
		string fileName;
		ConvertFromString<T> functie;
		ConvertToString<T> functieScriere;

		public FileRepository(String file, ConvertFromString<T> function, ConvertToString<T> scriere)
		{
			fileName = file;
			functie = function;
			functieScriere = scriere;
			readFromFile();
		}

		private void readFromFile()
		{
			using (TextReader reader = File.OpenText(fileName))
			{
				String line;
				while ((line = reader.ReadLine()) != null)
				{
					
					T b = functie(line);
					//Console.WriteLine("=====" + b);
					if (b!=null)
						base.save(b);
							}


			}
		}

		public override void save(T entity)
		{
			base.save(entity);
			writeToFile();
		}

		private void writeToFile()
		{
		using (TextWriter writer = File.CreateText(fileName))
			{
		foreach (T b in findAll())
		{
					writer.WriteLine(functieScriere(b));
		}
	}
		}
	}
}
