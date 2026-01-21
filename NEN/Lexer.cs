using System.Text.RegularExpressions;
using NEN.Types;

namespace NEN
{
    internal partial class Lexer
    {
        public static (string[], Token[]) Tokenize(string filePath)
        {
            var content = File.ReadAllText(filePath);
            //content = content.Replace("\t", "    ");
            var lines = content.Split(['\r', '\n'], StringSplitOptions.RemoveEmptyEntries);
            var regex = LexerRegex();
            List<Token> tokens = [];
            foreach (var (value, i) in lines.Select((value, i) => (value, i + 1)))
            {
                var matches = regex.Matches(value);
                foreach (Match match in matches)
                {
                    var matchType = GetMatchType(match);
                    tokens.Add(new Token { Type = matchType, Value = match.Value, Line = i, Column = match.Index + 1 });
                }
            }
            Lexer.Analyse(ref tokens);
            return (lines, [.. tokens]);
        }

        private static void Analyse(ref List<Token> tokens)
        {
            for (var i = 0; i < tokens.Count; i++)
            {
                switch (tokens[i].Type)
                {
                    case TokenType.Comment:
                        if (tokens[i].Value == "/*")
                        {
                            Lexer.RemoveComment(ref tokens, i);
                        }
                        else
                        {
                            tokens.RemoveAt(i);
                        }
                        i--;
                        break;
                    case TokenType.Unknown:
                        if (AnalyseKeyword(ref tokens.GetReferenceAt(i))) { }
                        else if (AnalyseLiteral(ref tokens.GetReferenceAt(i))) { }
                        else
                        {
                            tokens.GetReferenceAt(i).Type = TokenType.Identifier;
                        }
                        break;
                }
            }
        }
        private static bool AnalyseKeyword(ref Token token)
        {
            string[] keywords = [
                "lớp", "phương_thức", "quay_lại", "trả_về", "kết_thúc", "nhập", "biến", "hằng", "gán", "thuộc"
            ];
            if (keywords.Contains(token.Value))
            {
                token.Type = TokenType.Keyword;
                return true;
            }
            return false;
        }
        private static bool AnalyseLiteral(ref Token token)
        {
            // TODO: Add more literal types later
            if (Int32.TryParse(token.Value, out var _))
            {
                token.Type = TokenType.Literal;
                return true;
            }
            return false;
        }
        private static void RemoveComment(ref List<Token> tokens, int index)
        {
            while (index < tokens.Count && tokens[index].Value != "*/")
            {
                tokens.RemoveAt(index);
            }
        }

        [GeneratedRegex(@"(?<comment>//.*)|(?<comment>(/\*|\*/))|(?<literal>""[^""]+"")|(?<operator>\->)|(?<marker>@)|(?<operator>[+\-*\/=<>])|(?<punctuator>[,.();])|(?<unknown>[^\s,.();+\-*\/=<>@]+)")]
        private static partial Regex LexerRegex();
        private static Types.TokenType GetMatchType(Match match)
        {
            if (match.Groups["comment"].Success) return Types.TokenType.Comment;
            if (match.Groups["literal"].Success) return Types.TokenType.Literal;
            if (match.Groups["punctuator"].Success) return Types.TokenType.Punctuator;
            if (match.Groups["operator"].Success) return Types.TokenType.Operator;
            if (match.Groups["marker"].Success) return Types.TokenType.Marker;
            return Types.TokenType.Unknown;
        }
    }
    
    // Because List<T> is stupid lol
    public static class ListExtensions
    {
        // Safe helper to get a ref to a List<T> element
        public static ref T GetReferenceAt<T>(this List<T> list, int index)
        {
            ArgumentNullException.ThrowIfNull(list);
            if (index < 0 || index >= list.Count)
                throw new ArgumentOutOfRangeException(nameof(index));

            // Access the internal array via CollectionsMarshal
            return ref System.Runtime.InteropServices.CollectionsMarshal.AsSpan(list)[index];
        }
    }
}
