using System.Diagnostics.SymbolStore;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

namespace NEN
{
    internal class Test
    {
        public static void Test1()
        {
            {
                AssemblyName aName = new("Common");
                PersistedAssemblyBuilder ab = new(aName, typeof(object).Assembly);
                ModuleBuilder mb = ab.DefineDynamicModule(aName.Name ?? "Common");
                TypeBuilder tb = mb.DefineType(
                    "Common",
                    TypeAttributes.Public | TypeAttributes.Class
                );
                Type intType = typeof(object).Assembly!.GetType("System.Int32")!;
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
                ab.Save("Common.dll");
            }


            {
                AssemblyName aName = new("Test");
                string runtimePath = Path.GetDirectoryName(typeof(object).Assembly.Location)!;
                PathAssemblyResolver resolver = new(Directory.GetFiles(runtimePath, "*.dll").Append("Common.dll"));
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

        public static void TestLexer()
        {
            (string[] lines, Types.Token[] tokens) = Lexer.Tokenize("Example sources\\Test.nen");
            PrintTokens(tokens);
        }

        private static void PrintTokens(Types.Token[] tokens)
        {
            Console.WriteLine("Kết quả Lexer:");
            int valuePadding = tokens.Select(token => token.Value.Length).Max();
            var topBar = $"{"Value".PadRight(valuePadding)} | {"Type",-10} | {"Line",-4} | {"Column",-4}";
            var topBarLine = new string('-', topBar.Length);
            Console.WriteLine($"{topBar}\n{topBarLine}");
            foreach (Types.Token token in tokens)
            {
                Console.WriteLine($"{token.Value.PadRight(valuePadding)} | {token.Type,-10} | {token.Line,-4} | {token.Column,-4}");
            }
        }

        public static void TestParser()
        {
            (string[] lines, Types.Token[] tokens) = Lexer.Tokenize("Example sources\\ClassTest.nen");
            PrintTokens(tokens);
            try
            {
                var parser = new Parser("ClassTest", lines, tokens);
                var module = parser.Parse();
                Console.WriteLine($"Parser result:\n{module}");
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }

        public static void TestAssembler()
        {
            (string[] lines, Types.Token[] tokens) = Lexer.Tokenize("Example sources\\ClassTest.nen");
            PrintTokens(tokens);
            try
            {
                var parser = new Parser("ClassTest", lines, tokens);
                var module = parser.Parse();
                Console.WriteLine($"Kết quả Parser:\n{module}");
                var assembler = new Assembler("ClassTest", lines, module, []);
                assembler.Assemble();
                Console.WriteLine($"Hoàn thành biên dịch! OK");
            }
            catch (NENException e)
            {
                Console.WriteLine(e.Message);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                Console.WriteLine(e.StackTrace);
            }
        }
    }
}
