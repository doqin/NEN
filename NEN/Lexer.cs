using System.Text.RegularExpressions;
using System.Linq;

namespace NEN
{

    public partial class Lexer
    {
        [Flags]
        public enum TokenType
        {
            Identifier = 1 << 0, // variables
            Keyword = 1 << 1, // reserved words
            Literal = 1 << 2, // numeric, logical, textual and reference literals (e.g. true, 6.02e23, "music")
            Operator = 1 << 3, // symbols that operate on arguments and produce results. (e.g. +, <, =)
            Punctuator = 1 << 4, // punctuation characters and paired delimiters. (e.g. }, (, ;)
            Comment = 1 << 5,
            Marker = 1 << 6,
            Unknown = 0 // for further analysis
        };

        public class Token
        {
            public required TokenType Type { get; set; }
            public required string Value { get; set; }
            public required int StartLine { get; set; }
            public required int StartColumn { get; set; }
            public required int EndLine { get; set; }
            public required int EndColumn { get; set; }
        }

        public static readonly string[] Keywords =
[
            "lớp", "phương_thức", "quay_lại", "trả_về", "kết_thúc", "sử_dụng", "biến",
            "hằng", "gán", "thuộc", "tạo", "thuộc_tính", "nếu", "thì", "không_thì",
            "trong_khi", "thoát", "phương_thức_khởi_tạo", "không_gian", "kế_thừa"
        ];

        /// <summary>
        /// Tokenizes the contents of the specified file into an array of tokens representing lexical elements.
        /// </summary>
        /// <remarks>Each token includes information about its type, value, and position within the file.
        /// The method analyzes all lines in the file and assigns line and column numbers to each token. If the file
        /// cannot be read, an exception will be thrown.</remarks>
        /// <param name="filePath">The path to the file to be tokenized. Must refer to a readable text file.</param>
        /// <returns>An array of tokens extracted from the file. The array will be empty if the file contains no recognizable
        /// tokens.</returns>
        public static Token[] Tokenize(string filePath)
        {
            var lines = File.ReadAllLines(filePath);
            var regex = LexerRegex();
            List<Token> tokens = [];
            foreach (var (value, i) in lines.Select((value, i) => (value, i + 1)))
            {
                var matches = regex.Matches(value);
                foreach (Match match in matches)
                {
                    var matchType = GetMatchType(match);
                    var startColumn = match.Index + 1;
                    var endColumn = startColumn + match.Value.Length - 1;
                    tokens.Add(new Token { 
                        Type = matchType, 
                        Value = match.Value, 
                        StartLine = i, 
                        StartColumn = startColumn,
                        EndLine = i,
                        EndColumn = endColumn
                    });
                }
            }
            Lexer.Analyse( tokens);
            return [.. tokens];
        }

        public static Token[] TokenizeFromText(string content)
        {
            var lines = content
                .Replace("\r\n", "\n")
                .Replace("\r", "\n")
                .Split('\n');
            var regex = LexerRegex();
            List<Token> tokens = [];
            foreach (var (value, i) in lines.Select((value, i) => (value, i + 1)))
            {
                var matches = regex.Matches(value);
                foreach (Match match in matches)
                {
                    var matchType = GetMatchType(match);
                    var startColumn = match.Index + 1;
                    var endColumn = startColumn + match.Value.Length - 1;
                    tokens.Add(new Token
                    {
                        Type = matchType,
                        Value = match.Value,
                        StartLine = i,
                        StartColumn = startColumn,
                        EndLine = i,
                        EndColumn = endColumn
                    });
                }
            }
            Lexer.Analyse(tokens);
            return [.. tokens];
        }

        private static void Analyse( List<Token> tokens)
        {
            for (var i = 0; i < tokens.Count; i++)
            {
                switch (tokens[i].Type)
                {
                    case TokenType.Comment:
                        if (tokens[i].Value == "/*")
                        {
                            Lexer.RemoveComment( tokens, i);
                        }
                        else
                        {
                            tokens.RemoveAt(i);
                        }
                        i--;
                        break;
                    case TokenType.Unknown:
                        if (AnalyseKeyword( tokens.GetReferenceAt(i))) { }
                        else if (AnalyseLiteral( tokens.GetReferenceAt(i))) { }
                        else
                        {
                            tokens.GetReferenceAt(i).Type = TokenType.Identifier;
                        }
                        break;
                }
            }
        }

        private static bool AnalyseKeyword( Token token)
        {
            
            if (Keywords.Contains(token.Value))
            {
                token.Type = TokenType.Keyword;
                return true;
            }
            return false;
        }
        private static bool AnalyseLiteral( Token token)
        {
            // TODO: Add more literal types later
            if (Int32.TryParse(token.Value, out var _))
            {
                token.Type = TokenType.Literal;
                return true;
            }
            return false;
        }
        private static void RemoveComment( List<Token> tokens, int index)
        {
            while (index < tokens.Count && tokens[index].Value != "*/")
            {
                tokens.RemoveAt(index);
            }
        }

        [GeneratedRegex(@"(?<comment>//.*)|(?<comment>(/\*|\*/))|(?<literal>(đúng|sai))|(?<literal>[0-9]+L)|(?<literal>[0-9]+)|(?<literal>""[^""]+"")|(?<operator>(\->|>=|<=|!=|và|hoặc))|(?<marker>@)|(?<punctuator>::)|(?<punctuator>[,.(){}\[\];])|(?<operator>[+\-*\/=<>:!])|(?<unknown>[^\s,.(){}\[\];+\-*\/=<>@:!]+)")]
        private static partial Regex LexerRegex();
        private static TokenType GetMatchType(Match match)
        {
            if (match.Groups["comment"].Success) return TokenType.Comment;
            if (match.Groups["literal"].Success) return TokenType.Literal;
            if (match.Groups["punctuator"].Success) return TokenType.Punctuator;
            if (match.Groups["operator"].Success) return TokenType.Operator;
            if (match.Groups["marker"].Success) return TokenType.Marker;
            return TokenType.Unknown;
        }
    }
    
    // Because List<T> is stupid lol
    public static class ListExtensions
    {
        // Safe helper to get a  to a List<T> element
        public static  T GetReferenceAt<T>(this List<T> list, int index)
        {
            ArgumentNullException.ThrowIfNull(list);
            if (index < 0 || index >= list.Count)
                throw new ArgumentOutOfRangeException(nameof(index));

            // Access the public array via CollectionsMarshal
            return  System.Runtime.InteropServices.CollectionsMarshal.AsSpan(list)[index];
        }
    }
}
