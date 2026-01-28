using NEN.Types;
using System.Reflection;

namespace NEN
{
    public partial class StaticAnalyzer
    {
        private void GenerateDefaultConstructor(ClassNode c)
        {
            var defaultConstructorBuilder = c.TypeBuilder!.DefineConstructor(
                MethodAttributes.Public, 
                CallingConventions.Standard, 
                []
            );
            var defaultConstructor = new ConstructorNode
            {
                ReturnTypeNode = CreateTypeNodeFromType(defaultConstructorBuilder!.DeclaringType!, c.Line, c.Column),
                ConstructorBuilder = defaultConstructorBuilder,
                Line = c.Line,
                Column = c.Column
            };
            if (c.Constructors != null) throw new("Internal error");
            c.Constructors = [defaultConstructor];
            currentMethod = c.GetDefaultConstructor();
            var objectType = module.CoreAssembly!.GetType("System.Object") ?? throw new("Internal error");
            defaultConstructor.Statements = [ 
                new ExpressionStatement {
                    Expression = new StandardMethodCallExpression {
                        ReturnTypeNode = CreateTypeNodeFromType(objectType, c.Line, c.Column),
                        Object = new ThisExpression {
                            ReturnTypeNode = new NamedType {
                                Namespaces = [],
                                Name = c.Name,
                                Line = c.Line,
                                Column = c.Column
                            },
                            Line = c.Line,
                            Column = c.Column
                        },
                        MethodName = "ObjectConstructor",
                        Arguments = [],
                        MethodInfo = objectType.GetConstructor(Type.EmptyTypes),
                        Line = c.Line,
                        Column = c.Column
                    },
                    Line = c.Line, 
                    Column = c.Column
                }, // Constructs an `object` type (the base for any reference type)
                ..c.Fields.Where(f => f.InitialValue != null).Select(f =>
                {
                    if (f.FieldAttributes.HasFlag(FieldAttributes.Static)) {
                        return new AssignmentStatement {
                            Destination = new StaticFieldAccessmentExpression {
                                ReturnTypeNode = f.Variable.TypeNode,
                                TypeNode = new NamedType {
                                    Namespaces = [], // TODO: change when namespaces are implemented
                                    Name = c.Name,
                                    Line = c.Line,
                                    Column = c.Column
                                },
                                FieldName = f.Variable.Name,
                                FieldInfo = f.FieldInfo,
                                IsLoading = false,
                                Line = f.Line,
                                Column = f.Column
                            },
                            Source = f.InitialValue!,
                            Line = f.Line,
                            Column = f.Column
                        };                    }
                    else {
                        return new AssignmentStatement {
                            Destination = new StandardFieldAccessmentExpression {
                                ReturnTypeNode = f.Variable.TypeNode,
                                Object = new ThisExpression {
                                    ReturnTypeNode = new NamedType {
                                        Namespaces = [],
                                        Name = c.Name,
                                        Line = f.Line,
                                        Column = f.Column
                                    },
                                    Line = f.Line,
                                    Column = f.Column,
                                },
                                FieldName = f.Variable.Name,
                                FieldInfo = f.FieldInfo,
                                IsLoading = false,
                                Line = f.Line,
                                Column = f.Column
                            },
                            Source = f.InitialValue!,
                            Line = f.Line,
                            Column = f.Column
                        };
                    }
                }) // assignments statements
            ];
            // Analyze the initial values
            foreach (var statement in defaultConstructor.Statements)
            {
                AnalyzeStatement(c, new(), statement);
            }
            if (!moduleConstructors.TryAdd((c.Name, []), defaultConstructor.ConstructorBuilder))
            {
                throw new("Internal error");
            }
            currentMethod = null;
        }
    }
}
