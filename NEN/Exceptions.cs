using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NEN
{

    namespace Exceptions
    {
        public abstract class NENException : Exception
        {
            public NENException() { }
            public NENException(string[] contentLines, string message, int line, int column) : base(CreateException(contentLines, message, line, column)) { }

            private static string CreateException(string[] contentLines, string message, int line, int column)
            {
                return $"Dòng {line} Cột {column} | {message}\n{contentLines[line - 1]}\n" + new string('~', column - 1) + '^';
            }
        }
        public class ExpectedException : NENException 
        {
            public ExpectedException() { }
            public ExpectedException(string[] contentLines, string expected, int line, int column) : base(contentLines, $"Mong đợi {expected} tại vị trí này", line, column) { }
        }

        public class UnexpectedException : NENException
        {
            public UnexpectedException() { }
            public UnexpectedException(string[] contentLines, string unexpected, int line, int column) : base(contentLines, $"Không mong đợi '{unexpected}' tại vị trí này", line, column) { }
        }

        public class RedefinedException : NENException
        {
            public RedefinedException() { }
            public RedefinedException(string[] contentLines, string redefined, int line, int column) : base(contentLines, $"'{redefined}' đã được định nghĩa rồi", line, column) { }
        }

        public class MultipleEntryPointException : NENException
        {
            public MultipleEntryPointException() { }
            public MultipleEntryPointException(string[] contentLines, int line, int column) : base(contentLines, "Hàm chính được định nghĩa nhiều lần", line, column) { }
        }

        public class UnresolvedTypeException : NENException
        {
            public UnresolvedTypeException() { }
            public UnresolvedTypeException(string[] contentLines, string unresolvedType, int line, int column) : base(contentLines, $"Không thể tìm thấy định nghĩa của '{unresolvedType}'", line, column) { } 
        }

        public class UnresolvedIdentifierException : NENException
        {
            public UnresolvedIdentifierException() { }
            public UnresolvedIdentifierException(string[] contentLines, string unresolvedIdentifier, int line, int column) : base(contentLines, $"Không thể nhận diện được ký hiệu '{unresolvedIdentifier}'", line, column) { }
        }
    } 

}
