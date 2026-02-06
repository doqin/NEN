using NEN.AST;
using System.Reflection;

namespace NEN
{
    public partial class StaticAnalyzer
    {
        private void GenerateDefaultConstructor(ModulePart modulePart, ClassNode c)
        {
            var defaultConstructorBuilder = c.TypeBuilder!.DefineConstructor(
                MethodAttributes.Public, 
                CallingConventions.Standard, 
                []
            );
            var defaultConstructor = new ConstructorNode
            {
                DeclaringTypeNode = (CreateTypeNodeFromType(
                    defaultConstructorBuilder!.DeclaringType!, 
                    c.StartLine, 
                    c.StartColumn,
                    c.EndLine,
                    c.EndColumn) as NamedType)!,
                ConstructorBuilder = defaultConstructorBuilder,
                StartLine = c.StartLine,
                StartColumn = c.StartColumn,
                EndLine = c.EndLine,
                EndColumn = c.EndColumn
            };
            if (c.Constructors != null) throw new("Internal error");
            c.Constructors = [defaultConstructor];
            currentMethod = c.GetDefaultConstructor();
            var objectType = module.CoreAssembly!.GetType("System.Object") ?? throw new("Internal error");
            defaultConstructor.Statements = [ 
                ..c.Fields
                    .Where(
                        f => f.InitialValue != null && !f.FieldAttributes.HasFlag(FieldAttributes.Static)
                        )
                    .Select(f =>
                {
                    if (f.FieldAttributes.HasFlag(FieldAttributes.Static)) {
                        return new AssignmentStatement {
                            Destination = new StaticFieldAccessmentExpression {
                                ReturnTypeNode = f.Variable.TypeNode,
                                TypeNode = new NamedType {
                                    Namespaces = [], // TODO: change when namespaces are implemented
                                    Name = c.Name,
                                    StartLine = c.StartLine,
                                    StartColumn = c.StartColumn,
                                    EndLine = c.EndLine,
                                    EndColumn = c.EndColumn
                                },
                                FieldName = f.Variable.Name,
                                FieldInfo = f.FieldInfo,
                                IsLoading = false,
                                StartLine = f.StartLine,
                                StartColumn = f.StartColumn,
                                EndLine = f.EndLine,
                                EndColumn = f.EndColumn
                            },
                            Source = f.InitialValue!,
                            StartLine = f.StartLine,
                            StartColumn = f.StartColumn,
                            EndLine = f.EndLine,
                            EndColumn = f.EndColumn
                        };                    }
                    else {
                        return new AssignmentStatement {
                            Destination = new StandardFieldAccessmentExpression {
                                ReturnTypeNode = f.Variable.TypeNode,
                                Object = new ThisExpression {
                                    ReturnTypeNode = new NamedType {
                                        Namespaces = [],
                                        Name = c.Name,
                                        StartLine = f.StartLine,
                                        StartColumn = f.StartColumn,
                                        EndLine = f.EndLine,
                                        EndColumn = f.EndColumn
                                    },
                                    StartLine = f.StartLine,
                                    StartColumn = f.StartColumn,
                                    EndLine = f.EndLine,
                                    EndColumn = f.EndColumn,
                                },
                                FieldName = f.Variable.Name,
                                FieldInfo = f.FieldInfo,
                                IsLoading = false,
                                StartLine = f.StartLine,
                                StartColumn = f.StartColumn,
                                EndLine = f.EndLine,
                                EndColumn = f.EndColumn
                            },
                            Source = f.InitialValue!,
                            StartLine = f.StartLine,
                            StartColumn = f.StartColumn,
                            EndLine = f.EndLine,
                            EndColumn = f.EndColumn
                        };
                    }
                }) // assignments statements
            ];
            // Analyze the initial values
            foreach (var statement in defaultConstructor.Statements)
            {
                AnalyzeStatement(modulePart, c, [], [], statement);
            }
            if (!moduleConstructors.TryAdd((c.Name, []), defaultConstructor.ConstructorBuilder))
            {
                throw new("Internal error");
            }
            currentMethod = null;
        }
    }
}
