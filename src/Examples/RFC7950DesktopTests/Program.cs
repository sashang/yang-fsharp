using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RFC7950DesktopTests
{
    using Yang.Examples.RFC7950.SimpleModel;

    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Prefix:          {0}", Model.T.Information.Prefix);
            Console.WriteLine("Namespace:       {0}", Model.T.Information.Namespace);
            Console.WriteLine("Description:     {0}", Model.T.Information.Description);
            Console.WriteLine("Organization:    {0}", Model.T.Information.Organization);
            Console.WriteLine("Contact:         {0}", Model.T.Information.Contact);
            Console.WriteLine("Yang Version:    {0}", Model.T.Information.YangVersion);
        }
    }
}
