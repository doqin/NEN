// See https://aka.ms/new-console-template for more information
using NEN;
using NEN.AST;
using System.Reflection;
using System.Text;
using System.Text.Json;
using TBDNEN.Models;
using static NEN.Lexer;

namespace TBDNEN
{
    public class Program
    {
        public static void Main(string[] args)
        {
            Console.OutputEncoding = Encoding.UTF8;
            if (args.Length == 0)
            {
                Helper.PrintHelp();
                return;
            }
            string workingDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)!;
            if (args[0] == "xay")
            {
                try
                {
                    Build(workingDirectory);
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine(ex);
                }
            }
            else
            {
                Helper.PrintHelp();
            }
        }

        public static void Build(string workingDirectory)
        {
            string projFile = "";
            try
            {
                projFile = File.ReadAllText(Path.Combine(workingDirectory, "duannen.json"));
            }
            catch (Exception)
            {
                throw new("Không tìm thấy tệp duannen.json");
            }
            DuAnNen? projMetadata = null;
            try
            {
                projMetadata = JsonSerializer.Deserialize<DuAnNen>(projFile);
            }
            catch (Exception)
            {
                throw new("Tệp duannen.json không định dạng đúng cú pháp");
            }
            List<ModulePart> moduleParts = [];
            try
            {
                foreach(var file in projMetadata!.nguồn.Index())
                {
                    if (!Path.IsPathRooted(file.Item))
                    {
                        projMetadata.nguồn[file.Index] = Path.Combine(workingDirectory, file.Item);
                    }
                    if (!File.Exists(projMetadata.nguồn[file.Index])) throw new FileNotFoundException(null, projMetadata.nguồn[file.Index]);
                }
            }
            catch (FileNotFoundException f)
            {
                throw new($"Không tìm thấy tệp nào tên '{f.FileName}'");
            }
            Directory.CreateDirectory(Path.Combine(workingDirectory, projMetadata!.đích)!);
            foreach (var fileName in projMetadata!.nguồn)
            {
                Token[] tokens = Lexer.Tokenize(fileName);
                var parser = new Parser(fileName, tokens);
                var modulePart = parser.Parse();
                moduleParts.Add(modulePart);
            }
            var analyzer = new StaticAnalyzer(projMetadata.tên, [.. moduleParts], []);
            var module = analyzer.Analyze();
            var assembler = new Assembler(module, Path.Combine(workingDirectory, projMetadata!.đích)!);
            assembler.Assemble();
            Console.WriteLine($"Hoàn thành biên dịch! OK");
        }
    }
}
