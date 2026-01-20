using System.Collections.Immutable;

namespace NEN
{
    internal class Helper
    {
        public static string GetTreeString<T>(string head, T[] nodes)
        {
            var str = $"{head}\n";
            foreach (var (n, i) in nodes.Select((c, i) => (c, i)))
            {
                str += $"|\n";
                var strLines = (n?.ToString() ?? "").Split(['\r', '\n'], StringSplitOptions.RemoveEmptyEntries);
                foreach (var (line, j) in strLines.Select((line, j) => (line, j)))
                {
                    if (j == 0)
                    {
                        str += $"+- {line}\n";
                    }
                    else if (i < nodes.Length - 1)
                    {
                        str += $"|  {line}\n";
                    }
                    else
                    {
                        str += $"   {line}\n";
                    }
                }
            }
            return str;
        }
    }
}
