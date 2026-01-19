using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace NEN
{
    internal class Scanner
    {
        public static string[] Tokenize(string filePath)
        {
            var content = File.ReadAllText(filePath);
#pragma warning disable SYSLIB1045 // Convert to 'GeneratedRegexAttribute'.
            var regex = new Regex(@"""[^""]+""|[,.();""']|[^\s,.();""']+");
#pragma warning restore SYSLIB1045 // Convert to 'GeneratedRegexAttribute'.
            var matches = regex.Matches(content);
            return [.. matches.Select(m => m.Value)];
        }
    }
}
