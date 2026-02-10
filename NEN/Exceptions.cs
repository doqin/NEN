using NEN.AST;
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
            public string SourcePath;
            public int StartLine, StartColumn, EndLine, EndColumn;
            public string ErrorMessage;
            public NENException() {
                SourcePath = "";
                StartLine = 0;
                StartColumn = 0;
                EndLine = 0;
                EndColumn = 0;
                ErrorMessage = "";
            }
            public NENException(string sourcePath, string message, int startLine, int startColumn, int endLine, int endColumn) : base(CreateException(sourcePath, message, startLine, startColumn, endLine, endColumn)) 
            {
                SourcePath = sourcePath;
                StartLine = startLine;
                StartColumn = startColumn;
                EndLine = endLine;
                EndColumn = endColumn;
                ErrorMessage = message;
            }
            public NENException(string sourcePath, string message, string content, int startLine, int startColumn, int endLine, int endColumn) : base($"Tệp nguồn: {sourcePath}\nDòng {startLine} cột {startColumn}-{endColumn} | {message}\n{content}") 
            {
                SourcePath = sourcePath;
                StartLine = startLine;
                StartColumn = startColumn;
                EndLine = endLine;
                EndColumn = endColumn;
                ErrorMessage = message;
            }

            private static string CreateException(string sourcePath, string message, int startLine, int startColumn, int endLine, int endColumn)
            {
                // For now we assume the highlight is on a single line
                var caretCount = Math.Max(1, endColumn - startColumn + 1);
                return $"Tệp nguồn: {sourcePath}\nDòng {startLine} Cột {startColumn}-{endColumn} | {message}\n{Helper.ReadLine(sourcePath, startLine)}\n" + new string('~', startColumn - 1) + new string('^', caretCount);
            }
        }
        public class ExpectedException : NENException
        {
            public ExpectedException() { }
            public ExpectedException(string sourcePath, string expected, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Mong đợi {expected} tại vị trí này", startLine, startColumn, endLine, endColumn) { }
        }

        public class UnexpectedException : NENException
        {
            public UnexpectedException() { }
            public UnexpectedException(string sourcePath, string unexpected, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không mong đợi '{unexpected}' tại vị trí này", startLine, startColumn, endLine, endColumn) { }
        }

        public class RedefinedException : NENException
        {
            public RedefinedException() { }
            public RedefinedException(string sourcePath, string redefined, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"'{redefined}' đã được định nghĩa rồi", startLine, startColumn, endLine, endColumn) { }
        }

        public class MultipleEntryPointException : NENException
        {
            public MultipleEntryPointException() { }
            public MultipleEntryPointException(string sourcePath, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, "Hàm chính được định nghĩa nhiều lần", startLine, startColumn, endLine, endColumn) { }
        }

        public class UnresolvedTypeException : NENException
        {
            public UnresolvedTypeException() { }
            public UnresolvedTypeException(string sourcePath, string unresolvedType, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không thể tìm thấy định nghĩa của '{unresolvedType}'", startLine, startColumn, endLine, endColumn) { }
        }

        public class UnresolvedIdentifierException : NENException
        {
            public UnresolvedIdentifierException() { }
            public UnresolvedIdentifierException(string sourcePath, string unresolvedIdentifier, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không thể nhận diện được ký hiệu '{unresolvedIdentifier}'", startLine, startColumn, endLine, endColumn) { }
        }

        public class TypeDiscrepancyException : NENException
        {
            public TypeDiscrepancyException() { }
            public TypeDiscrepancyException(string sourcePath, TypeNode left, TypeNode right, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Kiểu dữ liệu không hợp lệ ({left} <-> {right})", CreateContent(sourcePath, left, right), startLine, startColumn, endLine, endColumn) { }

            private static string CreateContent(string sourcePath, TypeNode left, TypeNode right)
            {
                var leftCaretCount = Math.Max(1, left.EndColumn - left.StartColumn + 1);
                var rightCaretCount = Math.Max(1, right.EndColumn - right.StartColumn + 1);
                return $"{sourcePath[left.StartLine - 1]}\n" + new string('~', left.StartColumn - 1) + new string('^', leftCaretCount) + "\n" + $"{sourcePath[right.StartLine - 1]}\n" + new string('~', right.StartColumn - 1) + new string('^', rightCaretCount);
            }
        }

        public class MethodCallFromOutsideException : NENException
        {
            public MethodCallFromOutsideException() { }
            public MethodCallFromOutsideException(string sourcePath, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không thể gọi phương thức ngoài một phương thức", startLine, startColumn, endLine, endColumn) { }
        }

        public class StaticIllegalAccessmentException : NENException
        {
            public StaticIllegalAccessmentException() { }
            public StaticIllegalAccessmentException(string sourcePath, string accessment, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không thể truy cập thành phần không tĩnh '{accessment}' vì là phương thức tĩnh", startLine, startColumn, endLine, endColumn) { }
        }

        public class StandardMethodCallLikeStaticMethodException : NENException
        {
            public StandardMethodCallLikeStaticMethodException() { }
            public StandardMethodCallLikeStaticMethodException(string sourcePath, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, "Không thể gọi một phương thức có đối tượng như một phương thức tĩnh", startLine, startColumn, endLine, endColumn) { }
        }

        public class InvalidUsingStatement : NENException
        {
            public InvalidUsingStatement() { }
            public InvalidUsingStatement(string sourcePath, string typeName, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không thể sử dụng tên của một kiểu dữ liệu trong câu sử dụng không gian tên (Phát hiện kiểu dữ liệu {typeName})", startLine, startColumn, endLine, endColumn) { }
        }

        public class AmbiguousTypeUsage : NENException
        {
            public AmbiguousTypeUsage() { }
            public AmbiguousTypeUsage(string sourcePath, string typeName, string firstTypeName, string secondTypeName, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không thể phân biệt được kiểu dữ liệu đang sử dụng ({typeName} -> {firstTypeName}, {secondTypeName})", startLine, startColumn, endLine, endColumn) { }
        }

        public class IllegalAssignmentException : NENException
        {
            public IllegalAssignmentException() { }
            public IllegalAssignmentException(string sourcePath, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, "Không thể gán giá trị cho loại biểu thức này", startLine, startColumn, endLine, endColumn) { }
        }

        // Array exceptions

        public class InvalidArraySizeTypeException : NENException
        {
            public InvalidArraySizeTypeException() { }
            public InvalidArraySizeTypeException(string sourcePath, string typeName, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Kích thước mảng chỉ có thể là kiểu số nguyên (Biểu thức là kiểu {typeName})", startLine, startColumn, endLine, endColumn) { }
        }

        public class NegativeArraySizeException : NENException
        {
            public NegativeArraySizeException() { }
            public NegativeArraySizeException(string sourcePath, int size, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Kích thước mảng chỉ có thể là số nguyên dương (Kết quả biểu thước là {size})", startLine, startColumn, endLine, endColumn) { }
        }

        public class NoSizeArrayWithoutInitializationException : NENException
        {
            public NoSizeArrayWithoutInitializationException() { }
            public NoSizeArrayWithoutInitializationException(string sourcePath, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, "Mảng không có giá trị khởi tạo phải khai báo kích thước", startLine, startColumn, endLine, endColumn) { }
        }

        public class ArraySizeDiscrepancyException : NENException
        {
            public ArraySizeDiscrepancyException() { }
            public ArraySizeDiscrepancyException(string sourcePath, int declaredSize, int declaredStartLine, int declaredStartColumn, int declaredEndLine, int declaredEndColumn, int elementSize, int elementStartLine, int elementStartColumn, int elementEndLine, int elementEndColumn) : base(sourcePath, $"Kích thước mảng khai báo khác số phần tử được khởi tạo ({declaredSize} <-> {elementSize})", CreateContent(sourcePath, declaredStartLine, declaredStartColumn, declaredEndLine, declaredEndColumn, elementStartLine, elementStartColumn, elementEndLine, elementEndColumn), declaredStartLine, declaredStartColumn, declaredEndLine, declaredEndColumn) { }
            private static string CreateContent(string sourcePath, int leftStartLine, int leftStartColumn, int leftEndLine, int leftEndColumn, int rightStartLine, int rightStartColumn, int rightEndLine, int rightEndColumn)
            {
                var leftCaretCount = Math.Max(1, leftEndColumn - leftStartColumn + 1);
                var rightCaretCount = Math.Max(1, rightEndColumn - rightStartColumn + 1);
                return $"{sourcePath[leftStartLine - 1]}\n" + new string('~', leftStartColumn - 1) + new string('^', leftCaretCount) + "\n" + $"{sourcePath[rightStartLine - 1]}\n" + new string('~', rightStartColumn - 1) + new string('^', rightCaretCount);
            }
        }

        // Array indexing exceptions

        public class IndexingOnNonArrayException : NENException
        {
            public IndexingOnNonArrayException() { }
            public IndexingOnNonArrayException(string sourcePath, string typeName, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không thể truy cập phần tử lên một kiểu dữ liệu không phải là mảng (Kiểu dữ liệu {typeName})", startLine, startColumn, endLine, endColumn) { }
        }

        public class InvalidArrayIndexingTypeException : NENException
        {
            public InvalidArrayIndexingTypeException() { }
            public InvalidArrayIndexingTypeException(string sourcePath, string typeName, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Truy cập mảng chỉ chấp nhận kiểu số nguyên (Biểu thức là kiểu {typeName})", startLine, startColumn, endLine, endColumn) { }
        }

        // Field exceptions

        public class FieldInitializationOutsideDefaultConstructorException : NENException
        {
            public FieldInitializationOutsideDefaultConstructorException() { }
            public FieldInitializationOutsideDefaultConstructorException(string sourcePath, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không thể khởi tạo thuộc tính trực tiếp ở khai báo nếu phương thức khởi tạo đối tượng đã được định nghĩa", startLine, startColumn, endLine, endColumn) { }
        }

        public class InvalidFieldAccessmentException : NENException
        {
            public InvalidFieldAccessmentException() { }
            public InvalidFieldAccessmentException(string sourcePath, string fieldName, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không thể truy cập thuộc tính '{fieldName}' vì nó không tồn tại hoặc không công khai", startLine, startColumn, endLine, endColumn) { }
        }

        public class InvalidMethodCallException : NENException
        {
            public InvalidMethodCallException() { }
            public InvalidMethodCallException(string sourcePath, string methodName, Type[] signature, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không thể gọi phương thức {methodName}({string.Join(", ", signature.Select(t => t.FullName))}) vì nó không tồn tại hoặc không công khai", startLine, startColumn, endLine, endColumn) { }
        }

        public class UnresolvedConstructorException : NENException
        {
            public UnresolvedConstructorException() { }
            public UnresolvedConstructorException(string sourcePath, string typeName, Type[] signature, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Không thể tìm thấy phương thức khởi tạo {typeName}({string.Join(", ", signature.Select(t => t.FullName))})", startLine, startColumn, endLine, endColumn) { }
        }

        public class InvalidIfConditionTypeException : NENException { 
            public InvalidIfConditionTypeException() { } 
            public InvalidIfConditionTypeException(string sourcePath, string typeName, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Biểu thức điều kiện phải là kiểu lý luận (Biểu thức là kiểu {typeName})", startLine, startColumn, endLine, endColumn) { }
        }

        public class BreakOutsideLoopException : NENException
        {
            public BreakOutsideLoopException() { }
            public BreakOutsideLoopException(string sourcePath, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"'Thoát' chỉ có thể sử dụng trong một vòng lặp", startLine, startColumn, endLine, endColumn) { }
        }

        public class ImplicitTypeOnFieldException : NENException
        {
            public ImplicitTypeOnFieldException() { }
            public ImplicitTypeOnFieldException(string sourcePath, int startLine, int startColumn, int endLine, int endColumn) : base(sourcePath, $"Xét kiểu dữ liệu ngầm chỉ sử dụng được với biến cục bộ", startLine, startColumn, endLine, endColumn) { }
        }
    }
}
