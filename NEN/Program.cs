// See https://aka.ms/new-console-template for more information

using NEN;
using System.Text;

Console.OutputEncoding = Encoding.UTF8;

string[] tokens = Scanner.Tokenize("Example sources\\Test.nen");
foreach (string token in tokens)
{
    Console.WriteLine(token);
}