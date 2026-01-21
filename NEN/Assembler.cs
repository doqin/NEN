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
    internal class Assembler
    {
        private class SymbolTable
        {
            private readonly Dictionary<string, (LocalBuilder builder, int index)> _dict = [];
            private int _nextIndex = 0;

            public bool TryAdd(string key, LocalBuilder value)
            {
                if (_dict.TryAdd(key, (value, _nextIndex))) {
                    _nextIndex++;
                    return true;
                }
                return false;
            }

            public bool TryGetValue(string key, out LocalBuilder? value)
            {
                if (_dict.TryGetValue(key, out var entry)) {
                    value = entry.builder;
                    return true;
                }
                value = null;
                return false;
            }

            public bool TryGetIndex(string key, out int index)
            {
                if (_dict.TryGetValue(key, out var entry)) {
                    index = entry.index;
                    return true;
                }
                index = -1;
                return false;
            }
        }

        private readonly MetadataLoadContext metadataLoadContext;
        private readonly PersistedAssemblyBuilder assemblyBuilder;
        private readonly ModuleBuilder moduleBuilder;
        private readonly Assembly coreAssembly;
        private readonly AssemblyName assemblyName;
        private readonly NEN.Types.Module module;
        private readonly Dictionary<string, System.Type> typeTable = [];
        private MethodBuilder? entryPointMethod;
        private readonly string[] content;
        private readonly ISymbolDocumentWriter documentWriter;

        public Assembler(string assemblyName, string[] contentLines, NEN.Types.Module module, string[] assemblyPaths)
        {
            this.module = module;
            content = contentLines;
            string runtimePath = Path.GetDirectoryName(typeof(object).Assembly.Location)!;
            PathAssemblyResolver resolver = new([..Directory.GetFiles(runtimePath, "*.dll"), ..assemblyPaths]);
            metadataLoadContext = new(resolver);
            coreAssembly = metadataLoadContext.CoreAssembly!;
            this.assemblyName = new (assemblyName);
            assemblyBuilder = new(this.assemblyName!, coreAssembly!);
            moduleBuilder = assemblyBuilder.DefineDynamicModule(module.Name);
            documentWriter = moduleBuilder.DefineDocument($"{assemblyName}.nen");
        }

        public void Assemble()
        {
            SetupTargetFramework();
            foreach (var c in module.Classes)
            {
                AssembleType(c);
            }

            MetadataBuilder metadataBuilder = assemblyBuilder.GenerateMetadata(out BlobBuilder ilStream, out BlobBuilder fieldData, out MetadataBuilder pdbBuilder);
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
            using FileStream pdbStream = new($"{assemblyName.Name}.pdb", FileMode.Create, FileAccess.Write);
            portablePdbBlob.WriteContentTo(pdbStream);
            var debugDirectoryBuilder = new DebugDirectoryBuilder();
            debugDirectoryBuilder.AddCodeViewEntry($"{assemblyName.Name}.pdb", pdbContentId, portablePdbBuilder.FormatVersion);
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
            using FileStream fileStream = new($"{assemblyName.Name}.dll", FileMode.Create, FileAccess.Write);
            peBlob.WriteContentTo(fileStream);
        }

        private void CreateRuntimeConfig()
        {
            string configName = $"{assemblyName.Name}.runtimeconfig.json";
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

        private void AssembleType(Class c)
        {
            var typeBuilder = moduleBuilder.DefineType(
                c.Name,
                TypeAttributes.Public | TypeAttributes.Class
            );
            foreach(var method in c.Methods)
            {
                AssembleMethod(ref typeBuilder, method);
            }
            typeBuilder.CreateType();
        }

        private void AssembleMethod(ref TypeBuilder typeBuilder, Method method)
        {   
            var methodBuilder = typeBuilder.DefineMethod(
                method.Name, 
                method.Attributes,
                GetType(method.ReturnType.Name, method.ReturnType.Line, method.ReturnType.Column),
                null
            );
            if (method.IsEntryPoint)
            {
                if (entryPointMethod != null) throw new MultipleEntryPointException(content, method.Line, method.Column);
                entryPointMethod = methodBuilder;
            }
            var ilGenerator = methodBuilder.GetILGenerator();
            SymbolTable localSymbolTable = new();
            foreach(var statement in method.Statements)
            {
                AssembleStatement(ref ilGenerator, ref localSymbolTable, statement);
            }
            ilGenerator.Emit(OpCodes.Ret);
        }

        private void AssembleStatement(ref ILGenerator ilGenerator, ref SymbolTable localSymbolTable, Statement statement)
        {
            switch (statement)
            {
                case VariableDeclarationStatement variableDeclarationStatement:
                    AssembleVariableDeclarationStatement(ref ilGenerator, ref localSymbolTable, variableDeclarationStatement);
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        private void AssembleVariableDeclarationStatement(ref ILGenerator ilGenerator, ref SymbolTable localSymbolTable, VariableDeclarationStatement variableDeclarationStatement)
        {
            var variable = variableDeclarationStatement.Variable;
            var type = variable.Type;
            var localBuilder = ilGenerator.DeclareLocal(
                GetType(
                    type.Name,
                    type.Line,
                    type.Column
                )
            );
            localBuilder.SetLocalSymInfo(variable.Name);
            ilGenerator.MarkSequencePoint(documentWriter, variable.Line, variable.Column, variable.Line, variable.Column + 1);
            localSymbolTable.TryAdd(variable.Name, localBuilder);
            if (variableDeclarationStatement.InitialValue != null)
            {
                AssembleExpression(ref ilGenerator, ref localSymbolTable, variableDeclarationStatement.InitialValue);
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

        private void AssembleExpression(ref ILGenerator ilGenerator, ref SymbolTable localSymbolTable, Expression expression)
        {
            switch (expression)
            {
                case LiteralExpression literalExpression:
                    AssembleLiteralExpression(ref ilGenerator, ref localSymbolTable, literalExpression);
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        private void AssembleLiteralExpression(ref ILGenerator ilGenerator, ref SymbolTable localSymbolTable, LiteralExpression literalExpression)
        {
            if (literalExpression.Value.StartsWith('"') && literalExpression.Value.EndsWith('"'))
            {
                var value = literalExpression.Value;
                value = value.Remove(value.Length - 1, 1);
                value = value.Remove(0, 1);
                ilGenerator.Emit(OpCodes.Ldstr, value);
            }
            else
            {
                throw new NotImplementedException();
            }
        }

        /* Helpers */

        private System.Type GetType(string typeName, int line, int column)
        {
            if (typeTable.TryGetValue(typeName, out var result) == true)
            {
                return result;
            } 
            else
            {
                try
                {
                    System.Type type = coreAssembly.GetType(typeName)!;
                    typeTable.Add(typeName, type);
                    return type;
                }
                catch
                {
                    throw new UnresolvedTypeException(content, typeName, line, column);
                }
            }
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
            assemblyBuilder.SetCustomAttribute(targetFrameworkAttribute);
        }
    }
}
