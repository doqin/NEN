using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NEN
{
    namespace Symbols
    {
        public class SymbolComparer : IEqualityComparer<Symbol>
        {
            public bool Equals(Symbol? x, Symbol? y)
            {
                return StringComparer.InvariantCulture.Equals(x?.ToString(), y?.ToString());
            }

            public int GetHashCode([DisallowNull] Symbol obj)
            {
                return StringComparer.InvariantCulture.GetHashCode(obj.ToString()!);
            }
        }

        public enum SymbolKind
        {
            Class,
            Method,
            Field,
            Parameter,
            Local,
            Block
        }

        public readonly record struct SymbolSpan(int StartOffset, int EndOffset, SymbolKind Kind);

        public abstract class Symbol
        {
            public required string Name { get; set; }
        }
        public abstract class TypeSymbol : Symbol { }
        public class ClassSymbol : TypeSymbol
        {
            public override string ToString()
            {
                return Name;
            }
        }
        public class ANSISymbol : TypeSymbol
        {
            public override string ToString()
            {
                return Name;
            }
        }

        public class FieldSymbol : Symbol
        {
            public required TypeSymbol DeclaringType { get; set; }

            public override string ToString()
            {
                return $"{DeclaringType}::{Name}";
            }
        }
        public abstract class MethodBase : Symbol
        {
            public required TypeSymbol DeclaringType { get; set; }
            public required TypeSymbol[] Parameters { get; set; }
        }
        public class MethodSymbol : MethodBase {
            public override string ToString()
            {
                return $"{DeclaringType}::{Name}({string.Join(", ", Parameters.Select(p => p.ToString()))})";
            }
        }
        public class ConstructorSymbol : MethodBase {
            public override string ToString()
            {
                return $"{DeclaringType}::{Name}({string.Join(", ", Parameters.Select(p => p.ToString()))})";
            }
        }
        public class LocalSymbol : Symbol {
            public required MethodBase Method { get; set; }
            public override string ToString()
            {
                return $"{Method}::{Name}";
            }
        }
    }
}
