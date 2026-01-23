using NEN;
using System.Diagnostics.Metrics;
using System.Diagnostics.SymbolStore;
using System.IO.Enumeration;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Text;
Console.OutputEncoding = Encoding.UTF8;

namespace NENTest
{

    [TestClass]
    public sealed class NENTest
    {
        [TestMethod]
        public void LexerTest()
        {
            string fileName = "LexerTest";
            (string[] lines, NEN.Types.Token[] tokens) = NEN.Lexer.Tokenize($"Example sources\\{fileName}.nen");
            PrintTokens(tokens);
        }

        [TestMethod]
        public void ParserTest()
        {
            string fileName = "ParserTest";
            (string[] lines, NEN.Types.Token[] tokens) = Lexer.Tokenize($"Example sources\\{fileName}.nen");
            PrintTokens(tokens);
            var parser = new Parser(fileName, lines, tokens);
            var module = parser.Parse();
            Console.WriteLine($"Parser result:\n{module}");
        }

        [TestMethod]
        public void StaticAnalyzerTest()
        {
            string fileName = "ParserTest";
            (string[] lines, NEN.Types.Token[] tokens) = Lexer.Tokenize($"Example sources\\{fileName}.nen");
            PrintTokens(tokens);
            var parser = new Parser(fileName, lines, tokens);
            var module = parser.Parse();
            Console.WriteLine($"Parser result:\n{module}");
            var analyzer = new StaticAnalyzer(lines);
            analyzer.Analyze(ref module);
            Console.WriteLine($"Static Analyzer result:\n{module}");
        }

        [TestMethod]
        public void AssemblerTest()
        {
            string fileName = "AssemblerTest";
            (string[] lines, NEN.Types.Token[] tokens) = Lexer.Tokenize($"Example sources\\{fileName}.nen");
            PrintTokens(tokens);
            var parser = new Parser(fileName, lines, tokens);
            var module = parser.Parse();
            Console.WriteLine($"Kết quả Parser:\n{module}");
            var analyzer = new StaticAnalyzer(lines);
            analyzer.Analyze(ref module);
            var assembler = new Assembler(fileName, lines, module, []);
            assembler.Assemble();
            Console.WriteLine($"Hoàn thành biên dịch! OK");
        }

        private static void PrintTokens(NEN.Types.Token[] tokens)
        {
            Console.WriteLine("Kết quả Lexer:");
            int valuePadding = tokens.Select(token => token.Value.Length).Max();
            var topBar = $"{"Value".PadRight(valuePadding)} | {"Type",-10} | {"Line",-4} | {"Column",-4}";
            var topBarLine = new string('-', topBar.Length);
            Console.WriteLine($"{topBar}\n{topBarLine}");
            foreach (NEN.Types.Token token in tokens)
            {
                Console.WriteLine($"{token.Value.PadRight(valuePadding)} | {token.Type,-10} | {token.Line,-4} | {token.Column,-4}");
            }
        }
    }

    [TestClass]
    public sealed class ILTest
    {
        [TestMethod]
        public void GeneralTest()
        {
            {
                var (ab, context, coreAssembly) = CreateAssembly("Common", []);
                ModuleBuilder mb = ab.DefineDynamicModule("Common");
                TypeBuilder tb = mb.DefineType(
                    "Common",
                    TypeAttributes.Public | TypeAttributes.Class
                );
                Type intType = coreAssembly!.GetType("System.Int32")!;
                MethodBuilder sumMethod = tb.DefineMethod(
                    "Sum",
                    MethodAttributes.Public | MethodAttributes.Static,
                    intType,
                    [intType, intType]
                );

                ILGenerator il = sumMethod.GetILGenerator();
                il.Emit(OpCodes.Ldarg_S, 0);
                il.Emit(OpCodes.Ldarg_S, 1);
                il.Emit(OpCodes.Add);
                il.Emit(OpCodes.Ret);
                tb.CreateType();
                SaveAssembly(ab, "Common");
            }


            {
                var (ab, context, coreAssembly) = CreateAssembly("Test", ["Common.dll"]);
                ModuleBuilder mb = ab.DefineDynamicModule("Test");

                // Get types from MetadataLoadContext for IL generation
                Type consoleType = context.LoadFromAssemblyName("System.Console").GetType("System.Console")!;
                Type commonType = context.LoadFromAssemblyName("Common").GetType("Common")!;
                Type voidType = coreAssembly!.GetType("System.Void")!;
                Type stringType = coreAssembly!.GetType("System.String")!;
                Type intType = coreAssembly!.GetType("System.Int32")!;
                Type objectType = coreAssembly!.GetType("System.Object")!;
                Type variadicType = coreAssembly!.GetType("System.Object[]")!;

                TypeBuilder tb = mb.DefineType(
                    "Program",
                    TypeAttributes.Public | TypeAttributes.Class
                );

                MethodBuilder mainMethod = tb.DefineMethod(
                    "Main",
                    MethodAttributes.Public | MethodAttributes.Static,
                    voidType,
                    null
                );
                ISymbolDocumentWriter srcDoc = mb.DefineDocument("Test.nen");

                ILGenerator il = mainMethod.GetILGenerator();

                MethodInfo writeLineString = consoleType.GetMethod("WriteLine", [stringType])!;
                MethodInfo writeLineVariadic = consoleType.GetMethod("WriteLine", [stringType, variadicType])!;
                MethodInfo sum = commonType.GetMethod("Sum", [intType, intType])!;
                il.Emit(OpCodes.Ldstr, "Hello from .NET 9!");
                il.Emit(OpCodes.Call, writeLineString);
                il.Emit(OpCodes.Ldstr, "Testing testing!");
                il.Emit(OpCodes.Call, writeLineString);
                LocalBuilder myLB1 = il.DeclareLocal(intType);
                myLB1.SetLocalSymInfo("sumRet");
                il.Emit(OpCodes.Ldc_I4, 1);
                il.Emit(OpCodes.Ldc_I4, 1);
                il.Emit(OpCodes.Call, sum);
                il.Emit(OpCodes.Stloc_S, 0);

                //il.MarkSequencePoint(srcDoc, 4, 1, 4, 100);
                il.Emit(OpCodes.Ldstr, "{0} {1}");

                il.Emit(OpCodes.Ldc_I4, 2); // Push array size (1)
                il.Emit(OpCodes.Newarr, objectType); // Create array

                il.Emit(OpCodes.Dup); // Duplicate Array Ref for storing
                il.Emit(OpCodes.Ldc_I4, 0); // Load Index 0
                il.Emit(OpCodes.Ldstr, "sumRet:");
                // No boxing needed for string
                il.Emit(OpCodes.Stelem_Ref); // Store it in array

                il.Emit(OpCodes.Dup);
                il.Emit(OpCodes.Ldc_I4, 1);
                il.Emit(OpCodes.Ldloc_S, 0); // Push "sumRet" into index 1 of array
                il.Emit(OpCodes.Box, intType); // Box it
                il.Emit(OpCodes.Stelem_Ref);

                il.Emit(OpCodes.Call, writeLineVariadic);
                LocalBuilder myLB2 = il.DeclareLocal(intType);
                myLB2.SetLocalSymInfo("temp");
                il.Emit(OpCodes.Ldc_I4, 10);
                il.Emit(OpCodes.Stloc_S, 1);
                il.Emit(OpCodes.Ret);
                tb.CreateType();
                
                SaveAssemblyWithEntrypoint(ab, mainMethod, "Test");

                Console.WriteLine("Compilation Complete. Saved Test.dll");
                Console.WriteLine("Run with: dotnet Test.dll");
            }
        }

        [TestMethod]
        public void MethodCallTest()
        {
            var (ab, context, coreAssembly) = CreateAssembly("MethodCallTest", []);
            ModuleBuilder mb = ab.DefineDynamicModule("MethodCallTest");
            TypeBuilder tb = mb.DefineType("MethodCallTest", TypeAttributes.Public | TypeAttributes.Class);
            Type voidType = coreAssembly!.GetType("System.Void")!;
            Type consoleType = coreAssembly!.GetType("System.Console")!;
            Type stringType = coreAssembly!.GetType("System.String")!;
            MethodBuilder mainMethod = tb.DefineMethod(
                "Main", 
                MethodAttributes.Public | MethodAttributes.Static,
                voidType,
                null
            );

            MethodBuilder fooMethod = tb.DefineMethod(
                "Foo", 
                MethodAttributes.Public | MethodAttributes.Static,
                voidType,
                null
            );
            ILGenerator fooGen = fooMethod.GetILGenerator();
            MethodInfo writeLineMethod = consoleType.GetMethod("WriteLine", [stringType])!;
            fooGen.Emit(OpCodes.Ldstr, "Foo bar!");
            fooGen.Emit(OpCodes.Call, writeLineMethod);
            fooGen.Emit(OpCodes.Ret);
            ILGenerator mainGen = mainMethod.GetILGenerator();
            MethodInfo fooInfo = fooMethod;
            mainGen.Emit(OpCodes.Call, fooInfo);
            mainGen.Emit(OpCodes.Ret);
            tb.CreateType();
            SaveAssemblyWithEntrypoint(ab, mainMethod, "MethodCallTest");
        }

        private static void SaveAssemblyWithEntrypoint(PersistedAssemblyBuilder ab, MethodBuilder mainMethod, string assemblyName)
        {
            MetadataBuilder metadataBuilder = ab.GenerateMetadata(out BlobBuilder ilStream, out BlobBuilder fieldData, out MetadataBuilder pdbBuilder);
            PEHeaderBuilder peHeader = new(imageCharacteristics: Characteristics.ExecutableImage | Characteristics.Dll);

            // Build the PDB
            BlobBuilder portablePdbBlob = new();
            PortablePdbBuilder portablePdbBuilder = new(
                pdbBuilder,
                metadataBuilder.GetRowCounts(),
                MetadataTokens.MethodDefinitionHandle(mainMethod.MetadataToken)
            );
            BlobContentId pdbContentId = portablePdbBuilder.Serialize(portablePdbBlob);
            using FileStream pdbStream = new($"{assemblyName}.pdb", FileMode.Create, FileAccess.Write);
            portablePdbBlob.WriteContentTo(pdbStream);

            // Add debug directory with embedded PDB
            DebugDirectoryBuilder debugDirectoryBuilder = new();
            debugDirectoryBuilder.AddCodeViewEntry($"{assemblyName}.pdb", pdbContentId, portablePdbBuilder.FormatVersion);
            //debugDirectoryBuilder.AddEmbeddedPortablePdbEntry(portablePdbBlob, portablePdbBuilder.FormatVersion);

            ManagedPEBuilder peBuilder = new(
                header: peHeader,
                metadataRootBuilder: new MetadataRootBuilder(metadataBuilder),
                ilStream: ilStream,
                mappedFieldData: fieldData,
                debugDirectoryBuilder: debugDirectoryBuilder,
                entryPoint: MetadataTokens.MethodDefinitionHandle(mainMethod.MetadataToken)
            );

            BlobBuilder peBlob = new();
            peBuilder.Serialize(peBlob);
            using FileStream fileStream = new($"{assemblyName}.dll", FileMode.Create, FileAccess.Write);
            peBlob.WriteContentTo(fileStream);

            string configName = $"{assemblyName}.runtimeconfig.json";
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

        private static void SaveAssembly(PersistedAssemblyBuilder ab, string assemblyName)
        {
            MetadataBuilder metadataBuilder = ab.GenerateMetadata(out BlobBuilder ilStream, out BlobBuilder fieldData, out MetadataBuilder pdbBuilder);
            PEHeaderBuilder peHeader = new(imageCharacteristics: Characteristics.ExecutableImage | Characteristics.Dll);
            ManagedPEBuilder peBuilder = new(
                header: peHeader,
                metadataRootBuilder: new MetadataRootBuilder(metadataBuilder),
                ilStream: ilStream,
                mappedFieldData: fieldData,
                debugDirectoryBuilder: null,
                entryPoint: default
            );

            BlobBuilder peBlob = new();
            peBuilder.Serialize(peBlob);

            using FileStream fileStream = new($"{assemblyName}.dll", FileMode.Create, FileAccess.Write);
            peBlob.WriteContentTo(fileStream);

            string commonConfigName = $"{assemblyName}.runtimeconfig.json";
            string commonJsonContent = """
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
            File.WriteAllText(commonConfigName, commonJsonContent);
        }

        private static (PersistedAssemblyBuilder, MetadataLoadContext, Assembly?) CreateAssembly(string assemblyName, string[] includedLibraries)
        {
            AssemblyName aName = new(assemblyName);
            string runtimePath = Path.GetDirectoryName(typeof(object).Assembly.Location)!;
            PathAssemblyResolver resolver = new([..Directory.GetFiles(runtimePath, "*.dll"), ..includedLibraries]);
            MetadataLoadContext context = new(resolver);
            Assembly? coreAssembly = context.CoreAssembly;
            PersistedAssemblyBuilder ab = new(aName, coreAssembly ?? typeof(object).Assembly);

            // Set target framework attribute using runtime types for CustomAttributeBuilder
            ConstructorInfo targetFrameworkConstructor = typeof(System.Runtime.Versioning.TargetFrameworkAttribute).GetConstructor([typeof(string)])!;
            PropertyInfo frameworkDisplayNameProperty = typeof(System.Runtime.Versioning.TargetFrameworkAttribute).GetProperty("FrameworkDisplayName")!;

            CustomAttributeBuilder targetFrameworkAttribute = new(
                targetFrameworkConstructor,
                [".NETCoreApp,Version=v9.0"],
                [frameworkDisplayNameProperty],
                [".NET 9.0"]
            );
            ab.SetCustomAttribute(targetFrameworkAttribute);
            return (ab, context, coreAssembly);
        }

        [TestMethod]
        public void ConversionTest()
        {
            AssemblyName aName = new("Test");
            string runtimePath = Path.GetDirectoryName(typeof(object).Assembly.Location)!;
            PathAssemblyResolver resolver = new(Directory.GetFiles(runtimePath, "*.dll"));
            using MetadataLoadContext context = new(resolver);
            Assembly? coreAssembly = context.CoreAssembly;
            PersistedAssemblyBuilder ab = new(aName, coreAssembly ?? typeof(object).Assembly);

            // Set target framework attribute using runtime types for CustomAttributeBuilder
            ConstructorInfo targetFrameworkConstructor = typeof(System.Runtime.Versioning.TargetFrameworkAttribute).GetConstructor([typeof(string)])!;
            PropertyInfo frameworkDisplayNameProperty = typeof(System.Runtime.Versioning.TargetFrameworkAttribute).GetProperty("FrameworkDisplayName")!;

            CustomAttributeBuilder targetFrameworkAttribute = new(
                targetFrameworkConstructor,
                [".NETCoreApp,Version=v9.0"],
                [frameworkDisplayNameProperty],
                [".NET 9.0"]
            );
            ab.SetCustomAttribute(targetFrameworkAttribute);

            ModuleBuilder mb = ab.DefineDynamicModule(aName.Name ?? "Test");

            // Get types from MetadataLoadContext for IL generation
            Type consoleType = context.LoadFromAssemblyName("System.Console").GetType("System.Console")!;
            Type voidType = coreAssembly!.GetType("System.Void")!;
            Type stringType = coreAssembly!.GetType("System.String")!;
            Type int32Type = coreAssembly!.GetType("System.Int32")!;
            Type int64Type = coreAssembly!.GetType("System.Int64")!;
            Type objectType = coreAssembly!.GetType("System.Object")!;
            Type variadicType = coreAssembly!.GetType("System.Object[]")!;
            Type objectNullableType = coreAssembly!.GetType("System.Object?")!;

            TypeBuilder tb = mb.DefineType(
                "Program",
                TypeAttributes.Public | TypeAttributes.Class
            );

            MethodBuilder mainMethod = tb.DefineMethod(
                "Main",
                MethodAttributes.Public | MethodAttributes.Static,
                voidType,
                null
            );
            ISymbolDocumentWriter srcDoc = mb.DefineDocument("Test.nen");

            ILGenerator il = mainMethod.GetILGenerator();
            MethodInfo writeLine = consoleType.GetMethod("WriteLine", [int32Type])!;
            il.Emit(OpCodes.Ldc_I4, 2);
            il.Emit(OpCodes.Ldc_I4, 5);
            il.Emit(OpCodes.Add);
            il.Emit(OpCodes.Conv_I8);
            var local1 = il.DeclareLocal(int64Type);
            il.Emit(OpCodes.Stloc, 0);
            il.Emit(OpCodes.Ldc_I4, 2);
            var local2 = il.DeclareLocal(int64Type);
            il.Emit(OpCodes.Conv_I8);
            il.Emit(OpCodes.Stloc, 1);
            // il.Emit(OpCodes.Call, writeLine);
            il.Emit(OpCodes.Ret);
            tb.CreateType();

            MetadataBuilder metadataBuilder = ab.GenerateMetadata(out BlobBuilder ilStream, out BlobBuilder fieldData, out MetadataBuilder pdbBuilder);
            PEHeaderBuilder peHeader = new(imageCharacteristics: Characteristics.ExecutableImage | Characteristics.Dll);

            // Build the PDB
            BlobBuilder portablePdbBlob = new();
            PortablePdbBuilder portablePdbBuilder = new(
                pdbBuilder,
                metadataBuilder.GetRowCounts(),
                MetadataTokens.MethodDefinitionHandle(mainMethod.MetadataToken)
            );
            BlobContentId pdbContentId = portablePdbBuilder.Serialize(portablePdbBlob);

            // Save standalone PDB file
            using FileStream pdbStream = new("Test.pdb", FileMode.Create, FileAccess.Write);
            portablePdbBlob.WriteContentTo(pdbStream);

            // Add debug directory with embedded PDB
            DebugDirectoryBuilder debugDirectoryBuilder = new();
            debugDirectoryBuilder.AddCodeViewEntry("Test.pdb", pdbContentId, portablePdbBuilder.FormatVersion);
            //debugDirectoryBuilder.AddEmbeddedPortablePdbEntry(portablePdbBlob, portablePdbBuilder.FormatVersion);

            ManagedPEBuilder peBuilder = new(
                header: peHeader,
                metadataRootBuilder: new MetadataRootBuilder(metadataBuilder),
                ilStream: ilStream,
                mappedFieldData: fieldData,
                debugDirectoryBuilder: debugDirectoryBuilder,
                entryPoint: MetadataTokens.MethodDefinitionHandle(mainMethod.MetadataToken)
            );

            BlobBuilder peBlob = new();
            peBuilder.Serialize(peBlob);

            using FileStream fileStream = new("Test.dll", FileMode.Create, FileAccess.Write);
            peBlob.WriteContentTo(fileStream);

            string configName = "Test.runtimeconfig.json";
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

            Console.WriteLine("Compilation Complete. Saved Test.dll");
            Console.WriteLine("Run with: dotnet Test.dll");
        }
    }
}
