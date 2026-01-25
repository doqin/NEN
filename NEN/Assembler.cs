using NEN.Types;
using NEN.Exceptions;
using System;
using System.Diagnostics.SymbolStore;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

namespace NEN
{
    public class Assembler(string[] contentLines, Types.Module module)
    {
        private readonly Types.Module module = module;
        private MethodBuilder? entryPointMethod;
        private readonly string[] content = contentLines;
        private readonly ISymbolDocumentWriter documentWriter = module.ModuleBuilder!.DefineDocument($"{module.AssemblyBuilder!.GetName().Name}.nen");

        public void Assemble()
        {
            SetupTargetFramework();
            foreach (var c in module.Classes)
            {
                AssembleType(c);
            }

            MetadataBuilder metadataBuilder = module.AssemblyBuilder!.GenerateMetadata(out BlobBuilder ilStream, out BlobBuilder fieldData, out MetadataBuilder pdbBuilder);
            DebugDirectoryBuilder? debugDirectoryBuilder = null;
            if (entryPointMethod != null) debugDirectoryBuilder = CreateDebugMetadata(metadataBuilder, pdbBuilder);
            CreatePortableExecutable(metadataBuilder, ilStream, fieldData, debugDirectoryBuilder);
            CreateRuntimeConfig();
        }

        private DebugDirectoryBuilder CreateDebugMetadata(MetadataBuilder metadataBuilder, MetadataBuilder pdbBuilder)
        {
            PortablePdbBuilder portablePdbBuilder = new(
                pdbBuilder,
                metadataBuilder.GetRowCounts(),
                MetadataTokens.MethodDefinitionHandle(entryPointMethod!.MetadataToken)
            );
            BlobBuilder portablePdbBlob = new();
            BlobContentId pdbContentId = portablePdbBuilder.Serialize(portablePdbBlob);
            using FileStream pdbStream = new($"{module.AssemblyBuilder!.GetName().Name}.pdb", FileMode.Create, FileAccess.Write);
            portablePdbBlob.WriteContentTo(pdbStream);
            var debugDirectoryBuilder = new DebugDirectoryBuilder();
            debugDirectoryBuilder.AddCodeViewEntry($"{module.AssemblyBuilder!.GetName().Name}.pdb", pdbContentId, portablePdbBuilder.FormatVersion);
            return debugDirectoryBuilder;
        }

        private void CreatePortableExecutable(MetadataBuilder metadataBuilder, BlobBuilder ilStream, BlobBuilder fieldData, DebugDirectoryBuilder? debugDirectoryBuilder)
        {
            PEHeaderBuilder peHeader = new(imageCharacteristics: Characteristics.ExecutableImage | Characteristics.Dll);
            ManagedPEBuilder peBuilder = new(
                    header: peHeader,
                    metadataRootBuilder: new MetadataRootBuilder(metadataBuilder),
                    ilStream: ilStream,
                    mappedFieldData: fieldData,
                    debugDirectoryBuilder: debugDirectoryBuilder,
                    entryPoint: entryPointMethod == null ? default : MetadataTokens.MethodDefinitionHandle(entryPointMethod.MetadataToken)
                );
            BlobBuilder peBlob = new();
            peBuilder.Serialize(peBlob);
            using FileStream fileStream = new($"{module.AssemblyBuilder!.GetName().Name}.dll", FileMode.Create, FileAccess.Write);
            peBlob.WriteContentTo(fileStream);
        }

        private void CreateRuntimeConfig()
        {
            string configName = $"{module.AssemblyBuilder!.GetName().Name}.runtimeconfig.json";
            string jsonContent = """
                {
                    "runtimeOptions": {
                        "tfm": "net9.0",
                        "framework": {
                            "name": "Microsoft.NETCore.App",
                            "version": "9.0.0"
                        }
                    }
                }
                """;
            File.WriteAllText(configName, jsonContent);
        }

        private void AssembleType(ClassNode c)
        {
            foreach(var method in c.Methods)
            {
                AssembleMethod(c.TypeBuilder!, method);
            }
            c.TypeBuilder!.CreateType();
        }

        private void AssembleMethod( TypeBuilder typeBuilder, MethodNode method)
        {   
            if (method.IsEntryPoint)
            {
                if (entryPointMethod != null) throw new MultipleEntryPointException(content, method.Line, method.Column);
                entryPointMethod = method.MethodBuilder;
            }
            var ilGenerator = method.MethodBuilder!.GetILGenerator();
            SymbolTable<LocalBuilder> localSymbolTable = new();
            foreach(var statement in method.Statements)
            {
                VariableNode[] parameters = method.MethodBuilder.IsStatic ? method.Parameters : 
                    [ new VariableNode { 
                        Name = "này", 
                        Type = new TypeNode {
                            NamespaceAndName = typeBuilder.FullName!.Split("."),
                            Type = typeBuilder,
                            Line = method.ReturnType.Line,
                            Column = method.ReturnType.Column
                        }, 
                        Column = method.ReturnType.Line,
                        Line = method.ReturnType.Column
                    },..method.Parameters];
                AssembleStatement( ilGenerator, parameters, localSymbolTable, statement);
            }
            ilGenerator.Emit(OpCodes.Ret);
        }

        private void AssembleStatement( ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, StatementNode statement)
        {
            switch (statement)
            {
                case VariableDeclarationStatement variableDeclarationStatement:
                    AssembleVariableDeclarationStatement( ilGenerator, parameters, localSymbolTable, variableDeclarationStatement);
                    break;
                case ExpressionStatement expressionStatement:
                    AssembleExpressionStatement(ilGenerator, parameters, localSymbolTable, expressionStatement);
                    break;
                case AssignmentStatement assignmentStatement:
                    AssembleAssignmentStatement(ilGenerator, parameters, localSymbolTable, assignmentStatement);
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        private void AssembleAssignmentStatement(ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, AssignmentStatement assignmentStatement)
        {
            AssembleExpression(ilGenerator, parameters, localSymbolTable, assignmentStatement.Destination);
            AssembleExpression(ilGenerator, parameters, localSymbolTable, assignmentStatement.Source);
            switch (assignmentStatement.Destination)
            {
                case VariableExpression variableExpression:
                    for (int i = 0; i < parameters.Length; i++)
                    {
                        if (parameters[i].Name == variableExpression.Name)
                        {
                            ilGenerator.Emit(OpCodes.Starg_S, i);
                            return;
                        }
                    }
                    if (localSymbolTable.TryGetIndex(variableExpression.Name, out var localVariableIndex))
                    {
                        ilGenerator.Emit(OpCodes.Stloc_S, localVariableIndex);
                    }
                    else
                    {
                        // Usually should not happen as StaticAnalyzer already handles it
                        throw new UnresolvedIdentifierException(content, variableExpression.Name, variableExpression.Line, variableExpression.Column);
                    }
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        private void AssembleExpressionStatement(ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, ExpressionStatement expressionStatement)
        {
            AssembleExpression(ilGenerator, parameters, localSymbolTable, expressionStatement.Expression);
        }

        private void AssembleVariableDeclarationStatement( ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, VariableDeclarationStatement variableDeclarationStatement)
        {
            var variable = variableDeclarationStatement.Variable;
            var type = variable.Type;
            var localBuilder = ilGenerator.DeclareLocal(type.Type!);
            localBuilder.SetLocalSymInfo(variable.Name);
            ilGenerator.MarkSequencePoint(documentWriter, variable.Line, variable.Column, variable.Line, variable.Column + 1);
            localSymbolTable.TryAdd(variable.Name, localBuilder);
            if (variableDeclarationStatement.InitialValue != null)
            {
                AssembleExpression( ilGenerator, parameters, localSymbolTable, variableDeclarationStatement.InitialValue);
                if (localSymbolTable.TryGetIndex(variable.Name, out var index))
                {
                    ilGenerator.Emit(OpCodes.Stloc_S, index);
                }
                else
                {
                    throw new UnresolvedIdentifierException(content, variable.Name, variable.Line, variable.Column);
                }
            }
        }

        private void AssembleExpression( ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, ExpressionNode expression)
        {
            switch (expression)
            {
                case LiteralExpression literalExpression: AssembleLiteralExpression( ilGenerator, literalExpression); break;
                case VariableExpression variableExpression: AssembleVariableExpression( ilGenerator, parameters, localSymbolTable, variableExpression); break;
                case BinaryExpression binaryExpression: AssembleBinaryExpression( ilGenerator, parameters, localSymbolTable, binaryExpression); break;
                case StandardMethodCallExpression standardMethodCallExpression: AssembleStandardMethodCallExpression( ilGenerator, parameters, localSymbolTable, standardMethodCallExpression); break;
                case StaticMethodCallExpression staticMethodCallExpression: AssembleStaticMethodCallExpression(ilGenerator, parameters, localSymbolTable, staticMethodCallExpression); break;
                case ThisExpression: AssembleThisExpression(ilGenerator);  break;
                case BoxExpression boxExpression: AssembleBoxExpression(ilGenerator, parameters, localSymbolTable, boxExpression); break;
                default: throw new NotImplementedException();
            }
        }

        private void AssembleBoxExpression(ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, BoxExpression boxExpression)
        {
            AssembleExpression(ilGenerator, parameters, localSymbolTable, boxExpression.Expression);
            ilGenerator.Emit(OpCodes.Box, boxExpression.ReturnType!.Type!);
        }

        private void AssembleThisExpression(ILGenerator ilGenerator)
        {
            ilGenerator.Emit(OpCodes.Ldarg_0); // Load {this} instance
        }

        private void AssembleStaticMethodCallExpression(ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, StaticMethodCallExpression staticMethodCallExpression)
        {
            foreach (var argument in staticMethodCallExpression.Arguments)
            {
                AssembleExpression(ilGenerator, parameters, localSymbolTable, argument);
            }
            ilGenerator.Emit(OpCodes.Call, staticMethodCallExpression.Info!);
        }

        private void AssembleStandardMethodCallExpression(ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, StandardMethodCallExpression standardMethodCallExpression)
        {
            AssembleExpression(ilGenerator, parameters, localSymbolTable, standardMethodCallExpression.Object);
            foreach (var argument in standardMethodCallExpression.Arguments)
            {
                AssembleExpression(ilGenerator, parameters, localSymbolTable, argument);
            }
            ilGenerator.Emit(OpCodes.Call, standardMethodCallExpression.Info!);
        }

        private void AssembleVariableExpression( ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, VariableExpression variableExpression)
        {
            if (!variableExpression.IsLoading) return;
            for (int i = 0; i < parameters.Length; i++)
            {
                if (parameters[i].Name == variableExpression.Name)
                {
                    ilGenerator.Emit(OpCodes.Ldarg_S, i);
                    return;
                }
            }
            if (localSymbolTable.TryGetIndex(variableExpression.Name, out var localVariableIndex))
            {
                ilGenerator.Emit(OpCodes.Ldloc_S, localVariableIndex);
            }
            else
            {
                // Usually should not happen as StaticAnalyzer already handles it
                throw new UnresolvedIdentifierException(content, variableExpression.Name, variableExpression.Line, variableExpression.Column);
            }
        }

        private void AssembleBinaryExpression( ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, BinaryExpression binaryExpression)
        {
            AssembleExpression( ilGenerator, parameters, localSymbolTable, binaryExpression.Left);
            AssembleExpression( ilGenerator, parameters, localSymbolTable, binaryExpression.Right);
            if (binaryExpression.Operator == Operator.Plus)
            {
                ilGenerator.Emit(OpCodes.Add);
            }
            else if (binaryExpression.Operator == Operator.Minus)
            {
                ilGenerator.Emit(OpCodes.Sub);
            }
            else if (binaryExpression.Operator == Operator.Multiply)
            {
                ilGenerator.Emit(OpCodes.Mul);
            }
            else if (binaryExpression.Operator == Operator.Divide)
            {
                ilGenerator.Emit(OpCodes.Div);
            }
            else
            {
                throw new NotImplementedException();
            }
        }

        private void AssembleLiteralExpression( ILGenerator ilGenerator, LiteralExpression literalExpression)
        {
            if (literalExpression.ReturnType!.Type!.FullName == PrimitiveType.String)
            {
                ilGenerator.Emit(OpCodes.Ldstr, literalExpression.Value);
            }
            else if (literalExpression.ReturnType!.Type!.FullName == PrimitiveType.Int64)
            {
                ilGenerator.Emit(OpCodes.Ldc_I8, Int64.Parse(literalExpression.Value));
            }
            else if (literalExpression.ReturnType!.Type!.FullName == PrimitiveType.Int32)
            {
                ilGenerator.Emit(OpCodes.Ldc_I4, Int32.Parse(literalExpression.Value));
            }
            else
            {
                throw new NotImplementedException();
            }
        }

        /* Helpers */

        private void SetupTargetFramework()
        {
            ConstructorInfo targetFrameworkConstructor = typeof(System.Runtime.Versioning.TargetFrameworkAttribute).GetConstructor([typeof(string)])!;
            PropertyInfo frameworkDisplayNameProperty = typeof(System.Runtime.Versioning.TargetFrameworkAttribute).GetProperty("FrameworkDisplayName")!;

            CustomAttributeBuilder targetFrameworkAttribute = new(
                targetFrameworkConstructor,
                [".NETCoreApp,Version=v9.0"],
                [frameworkDisplayNameProperty],
                [".NET 9.0"]
            );
            module.AssemblyBuilder!.SetCustomAttribute(targetFrameworkAttribute);
        }
    }
}
