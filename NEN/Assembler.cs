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
    public class Assembler(Types.Module module, string savePath = "")
    {
        private readonly Types.Module module = module;
        private MethodBuilder? entryPointMethod;
        private readonly string savePath = string.IsNullOrWhiteSpace(savePath) ? "" : $"{savePath}/";
        // private readonly ISymbolDocumentWriter documentWriter = module.ModuleBuilder!.DefineDocument($"{module.AssemblyBuilder!.GetName().Name}.nen");

        public void Assemble()
        {
            SetupTargetFramework();
            foreach (var modulePart in module.ModuleParts)
            {
                foreach (var c in modulePart.Classes)
                {
                    AssembleType(modulePart, c);
                }
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
            using FileStream fileStream = new($"{savePath}{module.AssemblyBuilder!.GetName().Name}.dll", FileMode.Create, FileAccess.Write);
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
            File.WriteAllText($"{savePath}{configName}", jsonContent);
        }

        private void AssembleType(ModulePart modulePart, ClassNode c)
        {
            foreach(var constructor in c.Constructors ?? [])
            {
                AssembleConstructor(c.TypeBuilder, constructor);
            }
            foreach(var method in c.Methods)
            {
                AssembleMethod(modulePart, c.TypeBuilder!, method);
            }
            c.TypeBuilder!.CreateType();
        }

        private void AssembleConstructor(TypeBuilder? typeBuilder, ConstructorNode constructor)
        {
            var ilGenerator = constructor.ConstructorBuilder!.GetILGenerator();
            foreach(var statement in constructor.Statements)
            {
                AssembleStatement(ilGenerator, statement);
            }
            ilGenerator.Emit(OpCodes.Ret);
        }

        private void AssembleMethod(ModulePart modulePart, TypeBuilder typeBuilder, MethodNode method)
        {   
            if (method.IsEntryPoint)
            {
                if (entryPointMethod != null) throw new MultipleEntryPointException(modulePart.Source, method.Line, method.Column);
                entryPointMethod = method.MethodBuilder;
            }
            var ilGenerator = method.MethodBuilder!.GetILGenerator();
            foreach(var statement in method.Statements)
            {
                AssembleStatement(ilGenerator, statement);
            }
            ilGenerator.Emit(OpCodes.Ret);
        }

        private void AssembleStatement( ILGenerator ilGenerator, StatementNode statement)
        {
            switch (statement)
            {
                case VariableDeclarationStatement variableDeclarationStatement: AssembleVariableDeclarationStatement( ilGenerator, variableDeclarationStatement); break;
                case ExpressionStatement expressionStatement: AssembleExpressionStatement(ilGenerator, expressionStatement); break;
                case AssignmentStatement assignmentStatement: AssembleAssignmentStatement(ilGenerator, assignmentStatement); break;
                case ReturnStatement returnStatement: AssembleReturnStatement(ilGenerator, returnStatement); break;
                case IfStatement ifStatement: AssembleIfStatement(ilGenerator, ifStatement); break;
                case WhileStatement whileStatement: AssembleWhileStatement(ilGenerator, whileStatement); break;
                case BreakStatement breakStatement: AssembleBreakStatement(ilGenerator, breakStatement); break;
                default:
                    throw new NotImplementedException();
            }
        }

        private void AssembleBreakStatement(ILGenerator ilGenerator, BreakStatement breakStatement)
        {
            ilGenerator.Emit(OpCodes.Br, breakStatement.EndLabel!.Value);
        }

        private void AssembleWhileStatement(ILGenerator ilGenerator, WhileStatement whileStatement)
        {
            var startLabel = ilGenerator.DefineLabel();
            ilGenerator.MarkLabel(startLabel);
            AssembleExpression(ilGenerator, whileStatement.Condition);
            ilGenerator.Emit(OpCodes.Ldc_I4_1);
            ilGenerator.Emit(OpCodes.Bne_Un, whileStatement.EndLabel!.Value);
            foreach (var statement in whileStatement.Body)
            {
                AssembleStatement(ilGenerator, statement);
            }
            ilGenerator.Emit(OpCodes.Br, startLabel);
            ilGenerator.MarkLabel(whileStatement.EndLabel!.Value);
        }

        private void AssembleIfStatement(ILGenerator ilGenerator, IfStatement ifStatement)
        {
            var elseLabel = ilGenerator.DefineLabel();
            var endLabel = ilGenerator.DefineLabel();
            AssembleExpression(ilGenerator, ifStatement.Condition);
            ilGenerator.Emit(OpCodes.Ldc_I4_1);
            ilGenerator.Emit(OpCodes.Bne_Un, elseLabel);
            foreach (var statement in ifStatement.IfClause)
            {
                AssembleStatement(ilGenerator, statement);
            }
            ilGenerator.Emit(OpCodes.Br, endLabel);
            ilGenerator.MarkLabel(elseLabel);
            foreach (var statement in ifStatement.ElseClause)
            {
                AssembleStatement(ilGenerator, statement);
            }
            ilGenerator.Emit(OpCodes.Br, endLabel);
            ilGenerator.MarkLabel(endLabel);
        }

        private void AssembleReturnStatement(ILGenerator ilGenerator, ReturnStatement returnStatement)
        {
            if (returnStatement.Expression != null)
            {
                AssembleExpression(ilGenerator, returnStatement.Expression);
            }
            ilGenerator.Emit(OpCodes.Ret);
        }

        private void AssembleAssignmentStatement(ILGenerator ilGenerator, AssignmentStatement assignmentStatement)
        {
            AssembleExpression(ilGenerator, assignmentStatement.Destination);
            switch (assignmentStatement.Destination)
            {
                case VariableExpression variableExpression:
                    AssembleExpression(ilGenerator, assignmentStatement.Source);
                    ilGenerator.Emit(OpCodes.Stloc, variableExpression.LocalBuilder!);
                    break;
                case ArgumentExpression argumentExpression:
                    AssembleExpression(ilGenerator, assignmentStatement.Source);
                    ilGenerator.Emit(OpCodes.Starg, argumentExpression.Index!.Value);
                    break;
                case ArrayIndexingExpression arrayIndexingExpression:
                    var elementType = LowerTypeNode(arrayIndexingExpression.ReturnTypeNode!);
                    AssembleExpression(ilGenerator, arrayIndexingExpression.Index); // Load the index onto the stack
                    AssembleExpression(ilGenerator, assignmentStatement.Source);
                    if (elementType.IsValueType)
                    {
                        ilGenerator.Emit(OpCodes.Stelem, elementType); // Store the value onto the element specifed by the index
                    }
                    else
                    {
                        ilGenerator.Emit(OpCodes.Stelem_Ref);
                    }
                    break;
                case StandardFieldAccessmentExpression standardFieldAccessmentExpression:
                    AssembleExpression(ilGenerator, assignmentStatement.Source);
                    ilGenerator.Emit(OpCodes.Stfld, standardFieldAccessmentExpression.FieldInfo!);
                    break;
                case StaticFieldAccessmentExpression staticFieldAccessmentExpression:
                    AssembleExpression(ilGenerator, assignmentStatement.Source);
                    ilGenerator.Emit(OpCodes.Stsfld, staticFieldAccessmentExpression.FieldInfo!);
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        private void AssembleExpressionStatement(ILGenerator ilGenerator, ExpressionStatement expressionStatement)
        {
            AssembleExpression(ilGenerator, expressionStatement.Expression);
        }

        private void AssembleVariableDeclarationStatement( ILGenerator ilGenerator, VariableDeclarationStatement variableDeclarationStatement)
        {
            if (variableDeclarationStatement.InitialValue != null)
            {
                AssembleExpression( ilGenerator, variableDeclarationStatement.InitialValue);
                ilGenerator.Emit(OpCodes.Stloc_S, variableDeclarationStatement.LocalBuilder!);
            }
        }

        private void AssembleExpression( ILGenerator ilGenerator, ExpressionNode expression)
        {
            switch (expression)
            {
                case LiteralExpression literalExpression: AssembleLiteralExpression( ilGenerator, literalExpression); break;
                case VariableExpression variableExpression: AssembleVariableExpression( ilGenerator, variableExpression); break;
                case BinaryExpression binaryExpression: AssembleBinaryExpression( ilGenerator, binaryExpression); break;
                case StandardMethodCallExpression standardMethodCallExpression: AssembleStandardMethodCallExpression( ilGenerator, standardMethodCallExpression); break;
                case StaticMethodCallExpression staticMethodCallExpression: AssembleStaticMethodCallExpression(ilGenerator, staticMethodCallExpression); break;
                case ThisExpression: AssembleThisExpression(ilGenerator);  break;
                case DuplicateExpression _: AssembleDuplicateExpression(ilGenerator); break;
                case BoxExpression boxExpression: AssembleBoxExpression(ilGenerator, boxExpression); break;
                case NewArrayExpression newArrayExpression: AssembleNewArrayExpression(ilGenerator, newArrayExpression); break;
                case NewObjectExpression newObjectExpression: AssembleNewObjectExpression(ilGenerator, newObjectExpression); break;
                case ArrayIndexingExpression arrayIndexingExpression: AssembleArrayIndexingExpression(ilGenerator, arrayIndexingExpression); break;
                case StandardFieldAccessmentExpression standardFieldAccessmentExpression: AssembleStandardFieldAccessmentExpression(ilGenerator, standardFieldAccessmentExpression); break;
                case StaticFieldAccessmentExpression staticFieldAccessmentExpression: AssembleStaticFieldAccessmentExpression(ilGenerator, staticFieldAccessmentExpression); break;
                case ArgumentExpression argumentExpression: AssembleArgumentExpression(ilGenerator, argumentExpression); break;
                default: throw new NotImplementedException();
            }
        }

        private void AssembleArgumentExpression(ILGenerator ilGenerator, ArgumentExpression argumentExpression)
        {
            if (!argumentExpression.IsLoading) return;
            ilGenerator.Emit(OpCodes.Ldarg, argumentExpression.Index!.Value);
        }

        private void AssembleNewObjectExpression(ILGenerator ilGenerator, NewObjectExpression newObjectExpression)
        {
            ilGenerator.Emit(OpCodes.Newobj, newObjectExpression.ConstructorInfo!);
            foreach(var initialization in newObjectExpression.FieldInitializations)
            {
                AssembleAssignmentStatement(ilGenerator, initialization);
            }
        }

        private void AssembleStaticFieldAccessmentExpression(ILGenerator ilGenerator, StaticFieldAccessmentExpression staticFieldAccessmentExpression)
        {
            if (staticFieldAccessmentExpression.IsLoading == false) return;
            ilGenerator.Emit(OpCodes.Ldsfld, staticFieldAccessmentExpression.FieldInfo!);
        }

        private void AssembleStandardFieldAccessmentExpression(ILGenerator ilGenerator, StandardFieldAccessmentExpression standardFieldAccessmentExpression)
        {
            AssembleExpression(ilGenerator, standardFieldAccessmentExpression.Object);
            if (standardFieldAccessmentExpression.IsLoading == false) return;
            ilGenerator.Emit(OpCodes.Ldfld, standardFieldAccessmentExpression.FieldInfo!);
        }

        private void AssembleDuplicateExpression(ILGenerator ilGenerator)
        {
            ilGenerator.Emit(OpCodes.Dup);
        }

        private void AssembleArrayIndexingExpression(ILGenerator ilGenerator, ArrayIndexingExpression arrayIndexingExpression)
        {
            AssembleExpression(ilGenerator, arrayIndexingExpression.Array); // Load the array onto the stack
            if (!arrayIndexingExpression.IsLoading) return; // Don't load unless specified
            AssembleExpression(ilGenerator, arrayIndexingExpression.Index); // Load the index onto the stack
            Type elementType = LowerTypeNode(arrayIndexingExpression.ReturnTypeNode!);
            if (elementType.IsValueType)
            {
                ilGenerator.Emit(OpCodes.Ldelem, elementType);
            }
            else
            {
                ilGenerator.Emit(OpCodes.Ldelem_Ref);
            }
        }

        private void AssembleNewArrayExpression(ILGenerator ilGenerator, NewArrayExpression newArrayExpression)
        {
            AssembleExpression(ilGenerator, newArrayExpression.Size!);
            Type arrayElementType = LowerTypeNode(((ArrayType)newArrayExpression.ReturnTypeNode!).ElementTypeNode);
            ilGenerator.Emit(OpCodes.Newarr, arrayElementType);
            for (int i = 0; i < newArrayExpression.Elements.Length; i++)
            {
                ilGenerator.Emit(OpCodes.Dup); // Load a reference to the array onto the stack
                ilGenerator.Emit(OpCodes.Ldc_I4, i); // Load the index onto the stack
                AssembleExpression(ilGenerator, newArrayExpression.Elements[i]); // Load value or reference onto the stack
                Type elementType = LowerTypeNode(newArrayExpression.Elements[i].ReturnTypeNode!);
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

        private Type GetTypeFromName(ModulePart modulePart, string typeName, int line, int column)
        {
            return module.CoreAssembly!.GetType(typeName) ?? throw new UnresolvedTypeException(modulePart.Source, typeName, line, column);
        }

        private void AssembleBoxExpression(ILGenerator ilGenerator, BoxExpression boxExpression)
        {
            AssembleExpression(ilGenerator, boxExpression.Expression);
            ilGenerator.Emit(OpCodes.Box, LowerTypeNode(boxExpression.ReturnTypeNode!));
        }

        private void AssembleThisExpression(ILGenerator ilGenerator)
        {
            ilGenerator.Emit(OpCodes.Ldarg_0); // Load {this} instance
        }

        private void AssembleStaticMethodCallExpression(ILGenerator ilGenerator, StaticMethodCallExpression staticMethodCallExpression)
        {
            foreach (var argument in staticMethodCallExpression.Arguments)
            {
                AssembleExpression(ilGenerator, argument);
            }
            switch(staticMethodCallExpression.MethodInfo!)
            {
                case MethodInfo methodInfo:
                    ilGenerator.Emit(OpCodes.Call, methodInfo!);
                    break;
                case ConstructorInfo constructorInfo:
                    ilGenerator.Emit(OpCodes.Call, constructorInfo!);
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        private void AssembleStandardMethodCallExpression(ILGenerator ilGenerator, StandardMethodCallExpression standardMethodCallExpression)
        {
            AssembleExpression(ilGenerator, standardMethodCallExpression.Object);
            foreach (var argument in standardMethodCallExpression.Arguments)
            {
                AssembleExpression(ilGenerator, argument);
            }
            switch (standardMethodCallExpression.MethodInfo!)
            {
                case MethodInfo methodInfo:
                    ilGenerator.Emit(OpCodes.Call, methodInfo!);
                    break;
                case ConstructorInfo constructorInfo:
                    ilGenerator.Emit(OpCodes.Call, constructorInfo!);
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        private void AssembleVariableExpression( ILGenerator ilGenerator, VariableExpression variableExpression)
        {
            if (!variableExpression.IsLoading) return;
            ilGenerator.Emit(OpCodes.Ldloc_S, variableExpression.LocalBuilder!);
        }

        private void AssembleBinaryExpression( ILGenerator ilGenerator, BinaryExpression binaryExpression)
        {
            AssembleExpression( ilGenerator, binaryExpression.Left);
            AssembleExpression( ilGenerator, binaryExpression.Right);
            if (binaryExpression.Operator == "+")
            {
                ilGenerator.Emit(OpCodes.Add);
            }
            else if (binaryExpression.Operator == "-")
            {
                ilGenerator.Emit(OpCodes.Sub);
            }
            else if (binaryExpression.Operator == "*")
            {
                ilGenerator.Emit(OpCodes.Mul);
            }
            else if (binaryExpression.Operator == "/")
            {
                ilGenerator.Emit(OpCodes.Div);
            }
            else if (binaryExpression.Operator == "và")
            {
                ilGenerator.Emit(OpCodes.And);
            }
            else if (binaryExpression.Operator == "hoặc")
            {
                ilGenerator.Emit(OpCodes.Or);
            }
            else if (binaryExpression.Operator == "<")
            {
                ilGenerator.Emit(OpCodes.Clt);
            }
            else if (binaryExpression.Operator == "<=")
            {
                ilGenerator.Emit(OpCodes.Clt);
                AssembleExpression(ilGenerator, binaryExpression.Left);
                AssembleExpression(ilGenerator, binaryExpression.Right);
                ilGenerator.Emit(OpCodes.Ceq);
                ilGenerator.Emit(OpCodes.Or);
            }
            else if (binaryExpression.Operator == ">")
            {
                ilGenerator.Emit(OpCodes.Cgt);
            }
            else if (binaryExpression.Operator == ">=")
            {
                ilGenerator.Emit(OpCodes.Cgt);
                AssembleExpression(ilGenerator, binaryExpression.Left);
                AssembleExpression(ilGenerator, binaryExpression.Right);
                ilGenerator.Emit(OpCodes.Ceq);
                ilGenerator.Emit(OpCodes.Or);
            }
            else if (binaryExpression.Operator == "=")
            {
                ilGenerator.Emit(OpCodes.Ceq);
            }
            else if (binaryExpression.Operator == "!=")
            {
                ilGenerator.Emit(OpCodes.Ceq);
                ilGenerator.Emit(OpCodes.Ldc_I4_1);
                ilGenerator.Emit(OpCodes.Xor);
            }
            else
            {
                throw new NotImplementedException();
            }
        }

        private void AssembleLiteralExpression( ILGenerator ilGenerator, LiteralExpression literalExpression)
        {
            if (LowerTypeNode(literalExpression.ReturnTypeNode!).FullName == PrimitiveType.String)
            {
                ilGenerator.Emit(OpCodes.Ldstr, literalExpression.Value);
            }
            else if (LowerTypeNode(literalExpression.ReturnTypeNode!).FullName == PrimitiveType.Int64)
            {
                ilGenerator.Emit(OpCodes.Ldc_I8, Int64.Parse(literalExpression.Value));
            }
            else if (LowerTypeNode(literalExpression.ReturnTypeNode!).FullName == PrimitiveType.Int32)
            {
                ilGenerator.Emit(OpCodes.Ldc_I4, Int32.Parse(literalExpression.Value));
            }
            else if (LowerTypeNode(literalExpression.ReturnTypeNode!).FullName == PrimitiveType.Boolean)
            {
                if (literalExpression.Value == "đúng")
                    ilGenerator.Emit(OpCodes.Ldc_I4_1);
                else if (literalExpression.Value == "sai")
                    ilGenerator.Emit(OpCodes.Ldc_I4_0);
                else throw new NotImplementedException();
            }
            else
            {
                throw new NotImplementedException();
            }
        }

        /* Helpers */

        static private Type LowerTypeNode(TypeNode typeNode)
        {
            return typeNode switch
            {
                NamedType namedType => namedType.CLRType!,
                ArrayType arrayType => LowerTypeNode(arrayType.ElementTypeNode).MakeArrayType(1),
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
