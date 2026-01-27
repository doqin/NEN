using NEN.Exceptions;
using NEN.Types;
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
                AssembleStatement( ilGenerator, method.Parameters, localSymbolTable, statement);
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
            switch (assignmentStatement.Destination)
            {
                case VariableExpression variableExpression:
                    AssembleExpression(ilGenerator, parameters, localSymbolTable, assignmentStatement.Source);
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
                case ArrayIndexingExpression arrayIndexingExpression:
                    var elementType = Lower(arrayIndexingExpression.ReturnTypeNode!);
                    AssembleExpression(ilGenerator, parameters, localSymbolTable, arrayIndexingExpression.Index); // Load the index onto the stack
                    AssembleExpression(ilGenerator, parameters, localSymbolTable, assignmentStatement.Source);
                    if (elementType.IsValueType)
                    {
                        ilGenerator.Emit(OpCodes.Stelem, elementType); // Store the value onto the element specifed by the index
                    }
                    else
                    {
                        ilGenerator.Emit(OpCodes.Stelem_Ref);
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
            var type = variable.TypeNode;
            var localBuilder = ilGenerator.DeclareLocal(Lower(type));
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
                case NewArrayExpression newArrayExpression: AssembleNewArrayExpression(ilGenerator, parameters, localSymbolTable, newArrayExpression); break;
                case ArrayIndexingExpression arrayIndexingExpression: AssembleArrayIndexingExpression(ilGenerator, parameters, localSymbolTable, arrayIndexingExpression); break;
                default: throw new NotImplementedException();
            }
        }

        private void AssembleArrayIndexingExpression(ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, ArrayIndexingExpression arrayIndexingExpression)
        {
            AssembleExpression(ilGenerator, parameters, localSymbolTable, arrayIndexingExpression.Array); // Load the array onto the stack
            if (!arrayIndexingExpression.IsLoading) return; // Don't load unless specified
            AssembleExpression(ilGenerator, parameters, localSymbolTable, arrayIndexingExpression.Index); // Load the index onto the stack
            Type elementType = Lower(arrayIndexingExpression.ReturnTypeNode!);
            if (elementType.IsValueType)
            {
                ilGenerator.Emit(OpCodes.Ldelem, elementType);
            }
            else
            {
                ilGenerator.Emit(OpCodes.Ldelem_Ref);
            }
        }

        private void AssembleNewArrayExpression(ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, NewArrayExpression newArrayExpression)
        {
            AssembleExpression(ilGenerator, parameters, localSymbolTable, newArrayExpression.Size!);
            Type arrayElementType = Lower(((ArrayType)newArrayExpression.ReturnTypeNode!).ElementTypeNode);
            ilGenerator.Emit(OpCodes.Newarr, arrayElementType);
            for (int i = 0; i < newArrayExpression.Elements.Length; i++)
            {
                ilGenerator.Emit(OpCodes.Dup); // Load a reference to the array onto the stack
                ilGenerator.Emit(OpCodes.Ldc_I4, i); // Load the index onto the stack
                AssembleExpression(ilGenerator, parameters, localSymbolTable, newArrayExpression.Elements[i]); // Load value or reference onto the stack
                Type elementType = Lower(newArrayExpression.Elements[i].ReturnTypeNode!);
                if (elementType.IsValueType && arrayElementType.IsValueType)
                {
                    var line = newArrayExpression.Elements[i].ReturnTypeNode!.Line;
                    var column = newArrayExpression.Elements[i].ReturnTypeNode!.Column;
                    ilGenerator.Emit(OpCodes.Stelem, elementType);
                }
                else
                {
                    ilGenerator.Emit(OpCodes.Stelem_Ref);
                }
            }
        }

        private Type GetTypeFromName(string typeName, int line, int column)
        {
            return module.CoreAssembly!.GetType(typeName) ?? throw new UnresolvedTypeException(content, typeName, line, column);
        }

        private void AssembleBoxExpression(ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, BoxExpression boxExpression)
        {
            AssembleExpression(ilGenerator, parameters, localSymbolTable, boxExpression.Expression);
            ilGenerator.Emit(OpCodes.Box, Lower(boxExpression.ReturnTypeNode!));
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
            ilGenerator.Emit(OpCodes.Call, staticMethodCallExpression.MethodInfo!);
        }

        private void AssembleStandardMethodCallExpression(ILGenerator ilGenerator, VariableNode[] parameters, SymbolTable<LocalBuilder> localSymbolTable, StandardMethodCallExpression standardMethodCallExpression)
        {
            AssembleExpression(ilGenerator, parameters, localSymbolTable, standardMethodCallExpression.Object);
            foreach (var argument in standardMethodCallExpression.Arguments)
            {
                AssembleExpression(ilGenerator, parameters, localSymbolTable, argument);
            }
            ilGenerator.Emit(OpCodes.Call, standardMethodCallExpression.MethodInfo!);
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
            if (Lower(literalExpression.ReturnTypeNode!).FullName == PrimitiveType.String)
            {
                ilGenerator.Emit(OpCodes.Ldstr, literalExpression.Value);
            }
            else if (Lower(literalExpression.ReturnTypeNode!).FullName == PrimitiveType.Int64)
            {
                ilGenerator.Emit(OpCodes.Ldc_I8, Int64.Parse(literalExpression.Value));
            }
            else if (Lower(literalExpression.ReturnTypeNode!).FullName == PrimitiveType.Int32)
            {
                ilGenerator.Emit(OpCodes.Ldc_I4, Int32.Parse(literalExpression.Value));
            }
            else
            {
                throw new NotImplementedException();
            }
        }

        /* Helpers */

        static private Type Lower(TypeNode typeNode)
        {
            return typeNode switch
            {
                NamedType namedType => namedType.CLRType!,
                ArrayType arrayType => Lower(arrayType.ElementTypeNode).MakeArrayType(1),
                _ => throw new NotImplementedException()
            };
        }

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
