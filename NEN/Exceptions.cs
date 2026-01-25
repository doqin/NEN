using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations.Schema;
using System.Data.Common;
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
            public NENException(string message, string content, int line, int column) : base($"Dòng {line} cột {column} | {message}\n{content}") { }

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

        public class TypeDiscrepancyException : NENException
        {
            public TypeDiscrepancyException() { }
            public TypeDiscrepancyException(string[] contentLines, Types.TypeNode left, Types.TypeNode right, int line, int column) : base($"Kiểu dữ liệu không hợp lệ ({left} <-> {right})", CreateContent(contentLines, left, right), line, column) { }

            private static string CreateContent(string[] contentLines, Types.TypeNode left, Types.TypeNode right)
            {
                return $"{contentLines[left.Line - 1]}\n" + new string('~', left.Column - 1) + "^\n" + $"{contentLines[right.Line - 1]}\n" + new string('~', right.Column - 1) + '^';
            }
        }

        public class StaticIllegalAccessmentException : NENException
        {
            public StaticIllegalAccessmentException() { }
            public StaticIllegalAccessmentException(string[] contentLines, string accessment, int line, int column) : base(contentLines, $"Không thể truy cập thành phần không tĩnh '{accessment}' vì là phương thức tĩnh", line, column)
            {
            }
        }

        public class InvalidUsingStatement : NENException
        {
            public InvalidUsingStatement() { }
            public InvalidUsingStatement(string[] contentLines, string typeName, int line, int column) : base(contentLines, $"Không thể sử dụng tên của một kiểu dữ liệu trong câu sử dụng không gian tên (Phát hiện kiểu dữ liệu {typeName})", line, column) { }
        }

        public class AmbiguousTypeUsage : NENException
        {
            public AmbiguousTypeUsage() { }
            public AmbiguousTypeUsage(string[] contentLines, string typeName, string firstTypeName, string secondTypeName, int line, int column) : base(contentLines, $"Không thể phân biệt được kiểu dữ liệu đang sử dụng ({typeName} -> {firstTypeName}, {secondTypeName})", line, column) { }
        }
    } 

}
