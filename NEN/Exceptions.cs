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
            public NENException(string[] contentLines, string message, int startLine, int startColumn, int endLine, int endColumn) : base(CreateException(contentLines, message, startLine, startColumn, endLine, endColumn)) { }
            public NENException(string message, string content, int startLine, int startColumn, int endLine, int endColumn) : base($"Dòng {startLine} cột {startColumn}-{endColumn} | {message}\n{content}") { }

            private static string CreateException(string[] contentLines, string message, int startLine, int startColumn, int endLine, int endColumn)
            {
                // For now we assume the highlight is on a single line
                var caretCount = Math.Max(1, endColumn - startColumn + 1);
                return $"Dòng {startLine} Cột {startColumn}-{endColumn} | {message}\n{contentLines[startLine - 1]}\n" + new string('~', startColumn - 1) + new string('^', caretCount);
            }
        }
        public class ExpectedException : NENException
        {
            public ExpectedException() { }
            public ExpectedException(string[] contentLines, string expected, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Mong đợi {expected} tại vị trí này", startLine, startColumn, endLine, endColumn) { }
        }

        public class UnexpectedException : NENException
        {
            public UnexpectedException() { }
            public UnexpectedException(string[] contentLines, string unexpected, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không mong đợi '{unexpected}' tại vị trí này", startLine, startColumn, endLine, endColumn) { }
        }

        public class RedefinedException : NENException
        {
            public RedefinedException() { }
            public RedefinedException(string[] contentLines, string redefined, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"'{redefined}' đã được định nghĩa rồi", startLine, startColumn, endLine, endColumn) { }
        }

        public class MultipleEntryPointException : NENException
        {
            public MultipleEntryPointException() { }
            public MultipleEntryPointException(string[] contentLines, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, "Hàm chính được định nghĩa nhiều lần", startLine, startColumn, endLine, endColumn) { }
        }

        public class UnresolvedTypeException : NENException
        {
            public UnresolvedTypeException() { }
            public UnresolvedTypeException(string[] contentLines, string unresolvedType, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không thể tìm thấy định nghĩa của '{unresolvedType}'", startLine, startColumn, endLine, endColumn) { }
        }

        public class UnresolvedIdentifierException : NENException
        {
            public UnresolvedIdentifierException() { }
            public UnresolvedIdentifierException(string[] contentLines, string unresolvedIdentifier, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không thể nhận diện được ký hiệu '{unresolvedIdentifier}'", startLine, startColumn, endLine, endColumn) { }
        }

        public class TypeDiscrepancyException : NENException
        {
            public TypeDiscrepancyException() { }
            public TypeDiscrepancyException(string[] contentLines, Types.TypeNode left, Types.TypeNode right, int startLine, int startColumn, int endLine, int endColumn) : base($"Kiểu dữ liệu không hợp lệ ({left} <-> {right})", CreateContent(contentLines, left, right), startLine, startColumn, endLine, endColumn) { }

            private static string CreateContent(string[] contentLines, Types.TypeNode left, Types.TypeNode right)
            {
                var leftCaretCount = Math.Max(1, left.EndColumn - left.StartColumn + 1);
                var rightCaretCount = Math.Max(1, right.EndColumn - right.StartColumn + 1);
                return $"{contentLines[left.StartLine - 1]}\n" + new string('~', left.StartColumn - 1) + new string('^', leftCaretCount) + "\n" + $"{contentLines[right.StartLine - 1]}\n" + new string('~', right.StartColumn - 1) + new string('^', rightCaretCount);
            }
        }

        public class MethodCallFromOutsideException : NENException
        {
            public MethodCallFromOutsideException() { }
            public MethodCallFromOutsideException(string[] contentLines, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không thể gọi phương thức ngoài một phương thức", startLine, startColumn, endLine, endColumn) { }
        }

        public class StaticIllegalAccessmentException : NENException
        {
            public StaticIllegalAccessmentException() { }
            public StaticIllegalAccessmentException(string[] contentLines, string accessment, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không thể truy cập thành phần không tĩnh '{accessment}' vì là phương thức tĩnh", startLine, startColumn, endLine, endColumn) { }
        }

        public class InvalidUsingStatement : NENException
        {
            public InvalidUsingStatement() { }
            public InvalidUsingStatement(string[] contentLines, string typeName, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không thể sử dụng tên của một kiểu dữ liệu trong câu sử dụng không gian tên (Phát hiện kiểu dữ liệu {typeName})", startLine, startColumn, endLine, endColumn) { }
        }

        public class AmbiguousTypeUsage : NENException
        {
            public AmbiguousTypeUsage() { }
            public AmbiguousTypeUsage(string[] contentLines, string typeName, string firstTypeName, string secondTypeName, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không thể phân biệt được kiểu dữ liệu đang sử dụng ({typeName} -> {firstTypeName}, {secondTypeName})", startLine, startColumn, endLine, endColumn) { }
        }

        public class IllegalAssignmentException : NENException
        {
            public IllegalAssignmentException() { }
            public IllegalAssignmentException(string[] contentLines, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, "Không thể gán giá trị cho loại biểu thức này", startLine, startColumn, endLine, endColumn) { }
        }

        // Array exceptions

        public class InvalidArraySizeTypeException : NENException
        {
            public InvalidArraySizeTypeException() { }
            public InvalidArraySizeTypeException(string[] contentLines, string typeName, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Kích thước mảng chỉ có thể là kiểu số nguyên (Biểu thức là kiểu {typeName})", startLine, startColumn, endLine, endColumn) { }
        }

        public class NegativeArraySizeException : NENException
        {
            public NegativeArraySizeException() { }
            public NegativeArraySizeException(string[] contentLines, int size, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Kích thước mảng chỉ có thể là số nguyên dương (Kết quả biểu thước là {size})", startLine, startColumn, endLine, endColumn) { }
        }

        public class NoSizeArrayWithoutInitializationException : NENException
        {
            public NoSizeArrayWithoutInitializationException() { }
            public NoSizeArrayWithoutInitializationException(string[] contentLines, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, "Mảng không có giá trị khởi tạo phải khai báo kích thước", startLine, startColumn, endLine, endColumn) { }
        }

        public class ArraySizeDiscrepancyException : NENException
        {
            public ArraySizeDiscrepancyException() { }
            public ArraySizeDiscrepancyException(string[] contentLines, int declaredSize, int declaredStartLine, int declaredStartColumn, int declaredEndLine, int declaredEndColumn, int elementSize, int elementStartLine, int elementStartColumn, int elementEndLine, int elementEndColumn) : base($"Kích thước mảng khai báo khác số phần tử được khởi tạo ({declaredSize} <-> {elementSize})", CreateContent(contentLines, declaredStartLine, declaredStartColumn, declaredEndLine, declaredEndColumn, elementStartLine, elementStartColumn, elementEndLine, elementEndColumn), declaredStartLine, declaredStartColumn, declaredEndLine, declaredEndColumn) { }
            private static string CreateContent(string[] contentLines, int leftStartLine, int leftStartColumn, int leftEndLine, int leftEndColumn, int rightStartLine, int rightStartColumn, int rightEndLine, int rightEndColumn)
            {
                var leftCaretCount = Math.Max(1, leftEndColumn - leftStartColumn + 1);
                var rightCaretCount = Math.Max(1, rightEndColumn - rightStartColumn + 1);
                return $"{contentLines[leftStartLine - 1]}\n" + new string('~', leftStartColumn - 1) + new string('^', leftCaretCount) + "\n" + $"{contentLines[rightStartLine - 1]}\n" + new string('~', rightStartColumn - 1) + new string('^', rightCaretCount);
            }
        }

        // Array indexing exceptions

        public class IndexingOnNonArrayException : NENException
        {
            public IndexingOnNonArrayException() { }
            public IndexingOnNonArrayException(string[] contentLines, string typeName, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không thể truy cập phần tử lên một kiểu dữ liệu không phải là mảng (Kiểu dữ liệu {typeName})", startLine, startColumn, endLine, endColumn) { }
        }

        public class InvalidArrayIndexingTypeException : NENException
        {
            public InvalidArrayIndexingTypeException() { }
            public InvalidArrayIndexingTypeException(string[] contentLines, string typeName, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Truy cập mảng chỉ chấp nhận kiểu số nguyên (Biểu thức là kiểu {typeName})", startLine, startColumn, endLine, endColumn) { }
        }

        // Field exceptions

        public class FieldInitializationOutsideDefaultConstructorException : NENException
        {
            public FieldInitializationOutsideDefaultConstructorException() { }
            public FieldInitializationOutsideDefaultConstructorException(string[] contentLines, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không thể khởi tạo thuộc tính trực tiếp ở khai báo nếu phương thức khởi tạo đối tượng đã được định nghĩa", startLine, startColumn, endLine, endColumn) { }
        }

        public class InvalidFieldAccessmentException : NENException
        {
            public InvalidFieldAccessmentException() { }
            public InvalidFieldAccessmentException(string[] contentLines, string fieldName, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không thể truy cập thuộc tính '{fieldName}' vì nó không tồn tại hoặc không công khai", startLine, startColumn, endLine, endColumn) { }
        }

        public class InvalidMethodCallException : NENException
        {
            public InvalidMethodCallException() { }
            public InvalidMethodCallException(string[] contentLines, string methodName, Type[] signature, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không thể gọi phương thức {methodName}({string.Join(", ", signature.Select(t => t.FullName))}) vì nó không tồn tại hoặc không công khai", startLine, startColumn, endLine, endColumn) { }
        }

        public class UnresolvedConstructorException : NENException
        {
            public UnresolvedConstructorException() { }
            public UnresolvedConstructorException(string[] contentLines, string typeName, Type[] signature, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Không thể tìm thấy phương thức khởi tạo {typeName}({string.Join(", ", signature.Select(t => t.FullName))})", startLine, startColumn, endLine, endColumn) { }
        }

        public class InvalidIfConditionTypeException : NENException { 
            public InvalidIfConditionTypeException() { } 
            public InvalidIfConditionTypeException(string[] contentLines, string typeName, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"Biểu thức điều kiện phải là kiểu lý luận (Biểu thức là kiểu {typeName})", startLine, startColumn, endLine, endColumn) { }
        }

        public class BreakOutsideLoopException : NENException
        {
            public BreakOutsideLoopException() { }
            public BreakOutsideLoopException(string[] contentLines, int startLine, int startColumn, int endLine, int endColumn) : base(contentLines, $"'Thoát' chỉ có thể sử dụng trong một vòng lặp", startLine, startColumn, endLine, endColumn) { }
        }
    }
}
