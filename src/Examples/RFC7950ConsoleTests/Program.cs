using System;

namespace RFC7950ConsoleTests
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Prefix: {0}", Yang.Examples.RFC7950.SimpleModel.Model.T.Information.Prefix);
        }
    }
}
