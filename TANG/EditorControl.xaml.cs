using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.CodeCompletion;
using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Highlighting;
using ICSharpCode.AvalonEdit.Highlighting.Xshd;
using ICSharpCode.AvalonEdit.Rendering;
using ICSharpCode.AvalonEdit.Search;
using NEN;
using NEN.Types;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;
using System.Xml;
using TBDNEN.Models;

namespace TANG
{
    public partial class EditorControl : UserControl
    {
        public static readonly DependencyProperty TextProperty =
            DependencyProperty.Register(nameof(Text), typeof(string), typeof(EditorControl),
                new FrameworkPropertyMetadata(string.Empty,
                    FrameworkPropertyMetadataOptions.BindsTwoWayByDefault,
                    OnTextChanged));

        public static readonly DependencyProperty ShowLineNumbersProperty =
            DependencyProperty.Register(nameof(ShowLineNumbers), typeof(bool), typeof(EditorControl), new PropertyMetadata(true));

        public static readonly DependencyProperty WordWrapProperty =
            DependencyProperty.Register(nameof(WordWrap), typeof(bool), typeof(EditorControl), new PropertyMetadata(false));
        public static readonly DependencyProperty FilePathProperty =
            DependencyProperty.Register(nameof(FilePath), typeof(string), typeof(EditorControl), new PropertyMetadata(null, OnFilePathChanged));
        CompletionWindow? completionWindow;
        DispatcherTimer analysisTimer;
        SymbolColorizer? symbolColorizer;
        List<string> symbolCompletions = [];

        public EditorControl()
        {
            InitializeComponent();
            textEditor.TextChanged += (_, __) =>
            {
                if (Text != textEditor.Text)
                    Text = textEditor.Text;
                ScheduleAnalysis();
            };
            textEditor.TextArea.TextEntered += textEditor_TextArea_TextEntered;
            textEditor.TextArea.TextEntering += textEditor_TextArea_TextEntering;
            SearchPanel.Install(textEditor.TextArea);
            LoadHighlighting();
            analysisTimer = new DispatcherTimer
            {
                Interval = TimeSpan.FromMilliseconds(350)
            };
            analysisTimer.Tick += (_, __) =>
            {
                analysisTimer.Stop();
                RunSemanticHighlighting();
            };
            ScheduleAnalysis();
        }

        void textEditor_TextArea_TextEntered(object sender, TextCompositionEventArgs e)
        {
            if (!char.IsLetterOrDigit(e.Text[0])) return;
            var (wordStart, currentWord) = GetCurrentWord();

            completionWindow = new CompletionWindow(textEditor.TextArea)
            {
                StartOffset = wordStart,
                EndOffset = textEditor.TextArea.Caret.Offset
            };
            IList<ICompletionData> data = completionWindow.CompletionList.CompletionData;
            HashSet<string> added = new(StringComparer.Ordinal);
            foreach (var keyword in NEN.Lexer.Keywords)
            {
                if (added.Add(keyword))
                {
                    data.Add(new Types.NENCompletionData(keyword));
                }
            }
            foreach (var symbol in symbolCompletions)
            {
                if (added.Add(symbol))
                {
                    data.Add(new Types.NENCompletionData(symbol));
                }
            }

            if (!string.IsNullOrEmpty(currentWord))
            {
                completionWindow.CompletionList.SelectItem(currentWord);
            }

            completionWindow.Show();
            completionWindow.Closed += delegate
            {
                completionWindow = null;
            };
        }

        void textEditor_TextArea_TextEntering(object sender, TextCompositionEventArgs e)
        {
            if (e.Text.Length > 0 && completionWindow != null)
            {
                if (e.Text[0] == '\t')
                {
                    completionWindow.CompletionList.RequestInsertion(e);
                }
            }
        }

        private static void OnTextChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var control = (EditorControl)d;
            var newText = (string)e.NewValue ?? string.Empty;
            if (control.textEditor.Text != newText)
                control.textEditor.Text = newText;
            control.ScheduleAnalysis();
        }

        private static void OnFilePathChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var control = (EditorControl)d;
            control.ScheduleAnalysis();
        }

        public string Text
        {
            get => (string)GetValue(TextProperty);
            set => SetValue(TextProperty, value);
        }

        public bool ShowLineNumbers
        {
            get => (bool)GetValue(ShowLineNumbersProperty);
            set => SetValue(ShowLineNumbersProperty, value);
        }

        public bool WordWrap
        {
            get => (bool)GetValue(WordWrapProperty);
            set => SetValue(WordWrapProperty, value);
        }

        public string? FilePath
        {
            get => (string?)GetValue(FilePathProperty);
            set => SetValue(FilePathProperty, value);
        }

        void ScheduleAnalysis()
        {
            analysisTimer.Stop();
            analysisTimer.Start();
        }

        void RunSemanticHighlighting()
        {
            if (textEditor.Document == null)
            {
                ClearSymbolColorizer();
                return;
            }

            var content = textEditor.Text;
            if (string.IsNullOrWhiteSpace(content))
            {
                ClearSymbolColorizer();
                return;
            }

            try
            {
                List<ModulePart> moduleParts = [];
                string moduleName = "Trình soạn thảo";

                if (TryLoadProjectMetadata(out var project, out var projectRoot) && project != null)
                {
                    moduleName = string.IsNullOrWhiteSpace(project.tên) ? moduleName : project.tên;
                    foreach (var source in project.nguồn ?? [])
                    {
                        var resolvedPath = ResolveProjectPath(projectRoot, source);
                        if (string.IsNullOrWhiteSpace(resolvedPath)) continue;

                        (string[] lines, Token[] tokens) tokenized;

                        if (!string.IsNullOrWhiteSpace(FilePath) && string.Equals(resolvedPath, FilePath, StringComparison.OrdinalIgnoreCase))
                        {
                            tokenized = Lexer.TokenizeFromText(content);
                        }
                        else if (File.Exists(resolvedPath))
                        {
                            tokenized = Lexer.Tokenize(resolvedPath);
                        }
                        else
                        {
                            continue;
                        }

                        var parser = new Parser(resolvedPath, tokenized.lines, tokenized.tokens);
                        moduleParts.Add(parser.Parse());
                    }
                }

                if (moduleParts.Count == 0)
                {
                    var (lines, tokens) = Lexer.TokenizeFromText(content);
                    var parser = new Parser(FilePath ?? "Trình soạn thảo", lines, tokens);
                    moduleParts.Add(parser.Parse());
                }

                var analyzer = new StaticAnalyzer(moduleName, [.. moduleParts], []);
                var module = analyzer.Analyze();
                var spans = CollectSymbolSpans(module, FilePath);
                symbolCompletions = [.. CollectSymbolNames(module, FilePath)];
                ApplySymbolSpans(spans);
            }
            catch
            {
                ClearSymbolColorizer();
            }
        }

        bool TryLoadProjectMetadata(out DuAnNen? project, out string projectRoot)
        {
            project = null;
            projectRoot = string.Empty;

            if (string.IsNullOrWhiteSpace(FilePath)) return false;

            var dir = Path.GetDirectoryName(FilePath);
            while (!string.IsNullOrEmpty(dir))
            {
                var candidate = Path.Combine(dir, "duannen.json");
                if (File.Exists(candidate))
                {
                    try
                    {
                        var json = File.ReadAllText(candidate);
                        project = JsonSerializer.Deserialize<DuAnNen>(json, new JsonSerializerOptions { PropertyNameCaseInsensitive = true });
                        projectRoot = Path.GetDirectoryName(candidate)!;
                        return project != null;
                    }
                    catch
                    {
                        return false;
                    }
                }

                dir = Path.GetDirectoryName(dir);
            }

            return false;
        }

        static string? ResolveProjectPath(string projectRoot, string source)
        {
            if (string.IsNullOrWhiteSpace(source)) return null;
            if (Path.IsPathRooted(source)) return source;
            return Path.GetFullPath(Path.Combine(projectRoot, source));
        }

        IReadOnlyList<SymbolSpan> CollectSymbolSpans(Module module, string? currentPath)
        {
            List<SymbolSpan> spans = [];
            var currentFullPath = string.IsNullOrWhiteSpace(currentPath) ? null : Path.GetFullPath(currentPath);
            foreach (var modulePart in module.ModuleParts)
            {
                if (currentFullPath != null)
                {
                    var modulePartPath = Path.GetFullPath(modulePart.SourceName);
                    if (!string.Equals(modulePartPath, currentFullPath, StringComparison.OrdinalIgnoreCase))
                    {
                        continue;
                    }
                }
                foreach (var c in modulePart.Classes)
                {
                    AddSpan(spans, c.StartLine, c.StartColumn, c.EndLine, c.EndColumn, SymbolKind.Class);
                    foreach (var field in c.Fields)
                    {
                        AddSpan(spans, field.Variable.StartLine, field.Variable.StartColumn, field.Variable.EndLine, field.Variable.EndColumn, SymbolKind.Field);
                        AddTypeSpan(spans, field.Variable.TypeNode, SymbolKind.Class);
                        if (field.InitialValue != null)
                        {
                            CollectExpression(field.InitialValue, spans);
                        }
                    }

                    if (c.Constructors != null)
                    {
                        foreach (var ctor in c.Constructors)
                        {
                            AddSpan(spans, ctor.StartLine, ctor.StartColumn, ctor.EndLine, ctor.EndColumn, SymbolKind.Method);
                            AddTypeSpan(spans, ctor.ReturnTypeNode, SymbolKind.Class);
                            AddParametersAndStatements(spans, ctor.Parameters, ctor.Statements);
                        }
                    }

                    foreach (var method in c.Methods)
                    {
                        AddSpan(spans, method.StartLine, method.StartColumn, method.EndLine, method.EndColumn, SymbolKind.Method);
                        AddTypeSpan(spans, method.ReturnTypeNode, SymbolKind.Class);
                        AddParametersAndStatements(spans, method.Parameters, method.Statements);
                    }
                }
            }
            return spans;
        }

        IReadOnlyCollection<string> CollectSymbolNames(Module module, string? currentPath)
        {
            HashSet<string> names = new(StringComparer.Ordinal);
            var currentFullPath = string.IsNullOrWhiteSpace(currentPath) ? null : Path.GetFullPath(currentPath);
            foreach (var modulePart in module.ModuleParts)
            {
                if (currentFullPath != null)
                {
                    var modulePartPath = Path.GetFullPath(modulePart.SourceName);
                    if (!string.Equals(modulePartPath, currentFullPath, StringComparison.OrdinalIgnoreCase))
                    {
                        continue;
                    }
                }

                foreach (var c in modulePart.Classes)
                {
                    names.Add(c.Name);
                    foreach (var field in c.Fields)
                    {
                        names.Add(field.Variable.Name);
                    }

                    if (c.Constructors != null)
                    {
                        foreach (var ctor in c.Constructors)
                        {
                            names.Add(ctor.GetMethodInfo()?.Name ?? c.Name);
                            AddParametersNames(names, ctor.Parameters);
                            CollectStatementNames(ctor.Statements, names);
                        }
                    }

                    foreach (var method in c.Methods)
                    {
                        names.Add(method.MethodName);
                        AddParametersNames(names, method.Parameters);
                        CollectStatementNames(method.Statements, names);
                    }
                }
            }
            return names;
        }

        static void AddParametersNames(HashSet<string> names, IEnumerable<VariableNode> parameters)
        {
            foreach (var parameter in parameters)
            {
                names.Add(parameter.Name);
            }
        }

        void CollectStatementNames(IEnumerable<StatementNode> statements, HashSet<string> names)
        {
            foreach (var statement in statements)
            {
                switch (statement)
                {
                    case VariableDeclarationStatement variableDeclaration:
                        names.Add(variableDeclaration.Variable.Name);
                        if (variableDeclaration.InitialValue != null)
                        {
                            CollectExpressionNames(variableDeclaration.InitialValue, names);
                        }
                        break;
                    case AssignmentStatement assignment:
                        CollectExpressionNames(assignment.Destination, names);
                        CollectExpressionNames(assignment.Source, names);
                        break;
                    case ExpressionStatement expressionStatement:
                        CollectExpressionNames(expressionStatement.Expression, names);
                        break;
                    case ReturnStatement returnStatement when returnStatement.Expression != null:
                        CollectExpressionNames(returnStatement.Expression, names);
                        break;
                    case IfStatement ifStatement:
                        CollectExpressionNames(ifStatement.Condition, names);
                        CollectStatementNames(ifStatement.IfClause, names);
                        CollectStatementNames(ifStatement.ElseClause, names);
                        break;
                    case WhileStatement whileStatement:
                        CollectExpressionNames(whileStatement.Condition, names);
                        CollectStatementNames(whileStatement.Body, names);
                        break;
                }
            }
        }

        void CollectExpressionNames(ExpressionNode expression, HashSet<string> names)
        {
            switch (expression)
            {
                case VariableExpression variableExpression:
                    names.Add(variableExpression.Name);
                    break;
                case ArgumentExpression argumentExpression:
                    names.Add(argumentExpression.Name);
                    break;
                case StandardFieldAccessmentExpression fieldAccessmentExpression:
                    CollectExpressionNames(fieldAccessmentExpression.Object, names);
                    names.Add(fieldAccessmentExpression.FieldName);
                    break;
                case StaticFieldAccessmentExpression staticFieldAccessmentExpression:
                    names.Add(staticFieldAccessmentExpression.FieldName);
                    break;
                case StandardMethodCallExpression methodCallExpression:
                    CollectExpressionNames(methodCallExpression.Object, names);
                    names.Add(methodCallExpression.MethodName);
                    foreach (var argument in methodCallExpression.Arguments)
                    {
                        CollectExpressionNames(argument, names);
                    }
                    break;
                case StaticMethodCallExpression staticMethodCallExpression:
                    names.Add(staticMethodCallExpression.MethodName);
                    foreach (var argument in staticMethodCallExpression.Arguments)
                    {
                        CollectExpressionNames(argument, names);
                    }
                    break;
                case AmbiguousMethodCallExpression ambiguousMethodCallExpression:
                    names.Add(ambiguousMethodCallExpression.MethodName);
                    foreach (var argument in ambiguousMethodCallExpression.Arguments)
                    {
                        CollectExpressionNames(argument, names);
                    }
                    break;
                case BinaryExpression binaryExpression:
                    CollectExpressionNames(binaryExpression.Left, names);
                    CollectExpressionNames(binaryExpression.Right, names);
                    break;
                case NewArrayExpression newArrayExpression:
                    if (newArrayExpression.Size != null)
                    {
                        CollectExpressionNames(newArrayExpression.Size, names);
                    }
                    foreach (var element in newArrayExpression.Elements)
                    {
                        CollectExpressionNames(element, names);
                    }
                    break;
                case ArrayIndexingExpression arrayIndexingExpression:
                    CollectExpressionNames(arrayIndexingExpression.Array, names);
                    CollectExpressionNames(arrayIndexingExpression.Index, names);
                    break;
                case NewObjectExpression newObjectExpression:
                    foreach (var fieldInit in newObjectExpression.FieldInitializations)
                    {
                        CollectStatementNames([fieldInit], names);
                    }
                    break;
                case BoxExpression boxExpression:
                    CollectExpressionNames(boxExpression.Expression, names);
                    break;
            }
        }

        void AddParametersAndStatements(List<SymbolSpan> spans, IEnumerable<VariableNode> parameters, IEnumerable<StatementNode> statements)
        {
            foreach (var parameter in parameters)
            {
                AddSpan(spans, parameter.StartLine, parameter.StartColumn, parameter.EndLine, parameter.EndColumn, SymbolKind.Parameter);
                AddTypeSpan(spans, parameter.TypeNode, SymbolKind.Class);
            }

            CollectStatements(statements, spans);
        }

        void CollectStatements(IEnumerable<StatementNode> statements, List<SymbolSpan> spans)
        {
            foreach (var statement in statements)
            {
                switch (statement)
                {
                    case VariableDeclarationStatement variableDeclaration:
                        AddSpan(spans, variableDeclaration.Variable.StartLine, variableDeclaration.Variable.StartColumn, variableDeclaration.Variable.EndLine, variableDeclaration.Variable.EndColumn, SymbolKind.Local);
                        AddTypeSpan(spans, variableDeclaration.Variable.TypeNode, SymbolKind.Class);
                        if (variableDeclaration.InitialValue != null)
                        {
                            CollectExpression(variableDeclaration.InitialValue, spans);
                        }
                        break;
                    case AssignmentStatement assignment:
                        CollectExpression(assignment.Destination, spans);
                        CollectExpression(assignment.Source, spans);
                        break;
                    case ExpressionStatement expressionStatement:
                        CollectExpression(expressionStatement.Expression, spans);
                        break;
                    case ReturnStatement returnStatement when returnStatement.Expression != null:
                        CollectExpression(returnStatement.Expression, spans);
                        break;
                    case IfStatement ifStatement:
                        CollectExpression(ifStatement.Condition, spans);
                        CollectStatements(ifStatement.IfClause, spans);
                        CollectStatements(ifStatement.ElseClause, spans);
                        break;
                    case WhileStatement whileStatement:
                        CollectExpression(whileStatement.Condition, spans);
                        CollectStatements(whileStatement.Body, spans);
                        break;
                }
            }
        }

        void CollectExpression(ExpressionNode expression, List<SymbolSpan> spans)
        {
            switch (expression)
            {
                case VariableExpression variableExpression:
                    AddSpan(spans, variableExpression.StartLine, variableExpression.StartColumn, variableExpression.EndLine, variableExpression.EndColumn, SymbolKind.Local);
                    break;
                case ArgumentExpression argumentExpression:
                    AddSpan(spans, argumentExpression.StartLine, argumentExpression.StartColumn, argumentExpression.EndLine, argumentExpression.EndColumn, SymbolKind.Parameter);
                    break;
                case StandardFieldAccessmentExpression fieldAccessmentExpression:
                    AddSpan(spans, fieldAccessmentExpression.StartLine, fieldAccessmentExpression.StartColumn, fieldAccessmentExpression.EndLine, fieldAccessmentExpression.EndColumn, SymbolKind.Field);
                    CollectExpression(fieldAccessmentExpression.Object, spans);
                    break;
                case StaticFieldAccessmentExpression staticFieldAccessmentExpression:
                    AddSpan(spans, staticFieldAccessmentExpression.StartLine, staticFieldAccessmentExpression.StartColumn, staticFieldAccessmentExpression.EndLine, staticFieldAccessmentExpression.EndColumn, SymbolKind.Field);
                    AddTypeSpan(spans, staticFieldAccessmentExpression.TypeNode, SymbolKind.Class);
                    break;
                case StandardMethodCallExpression methodCallExpression:
                    AddSpan(spans, methodCallExpression.StartLine, methodCallExpression.StartColumn, methodCallExpression.EndLine, methodCallExpression.EndColumn, SymbolKind.Method);
                    CollectExpression(methodCallExpression.Object, spans);
                    foreach (var argument in methodCallExpression.Arguments)
                    {
                        CollectExpression(argument, spans);
                    }
                    break;
                case StaticMethodCallExpression staticMethodCallExpression:
                    AddSpan(spans, staticMethodCallExpression.StartLine, staticMethodCallExpression.StartColumn, staticMethodCallExpression.EndLine, staticMethodCallExpression.EndColumn, SymbolKind.Method);
                    AddTypeSpan(spans, staticMethodCallExpression.TypeNode, SymbolKind.Class);
                    foreach (var argument in staticMethodCallExpression.Arguments)
                    {
                        CollectExpression(argument, spans);
                    }
                    break;
                case AmbiguousMethodCallExpression ambiguousMethodCallExpression:
                    AddSpan(spans, ambiguousMethodCallExpression.StartLine, ambiguousMethodCallExpression.StartColumn, ambiguousMethodCallExpression.EndLine, ambiguousMethodCallExpression.EndColumn, SymbolKind.Method);
                    foreach (var argument in ambiguousMethodCallExpression.Arguments)
                    {
                        CollectExpression(argument, spans);
                    }
                    break;
                case BinaryExpression binaryExpression:
                    CollectExpression(binaryExpression.Left, spans);
                    CollectExpression(binaryExpression.Right, spans);
                    break;
                case NewArrayExpression newArrayExpression:
                    if (newArrayExpression.Size != null)
                    {
                        CollectExpression(newArrayExpression.Size, spans);
                    }
                    foreach (var element in newArrayExpression.Elements)
                    {
                        CollectExpression(element, spans);
                    }
                    AddTypeSpan(spans, newArrayExpression.ReturnTypeNode, SymbolKind.Class);
                    break;
                case ArrayIndexingExpression arrayIndexingExpression:
                    CollectExpression(arrayIndexingExpression.Array, spans);
                    CollectExpression(arrayIndexingExpression.Index, spans);
                    break;
                case NewObjectExpression newObjectExpression:
                    AddTypeSpan(spans, newObjectExpression.ReturnTypeNode, SymbolKind.Class);
                    foreach (var fieldInit in newObjectExpression.FieldInitializations)
                    {
                        CollectStatements([fieldInit], spans);
                    }
                    break;
                case BoxExpression boxExpression:
                    CollectExpression(boxExpression.Expression, spans);
                    break;
            }
        }

        void AddTypeSpan(List<SymbolSpan> spans, TypeNode? typeNode, SymbolKind kind)
        {
            if (typeNode == null) return;
            AddSpan(spans, typeNode.StartLine, typeNode.StartColumn, typeNode.EndLine, typeNode.EndColumn, kind);
            if (typeNode is ArrayType arrayType)
            {
                AddTypeSpan(spans, arrayType.ElementTypeNode, kind);
            }
        }

        void AddSpan(List<SymbolSpan> spans, int startLine, int startColumn, int endLine, int endColumn, SymbolKind kind)
        {
            if (startLine < 1 || startColumn < 1 || endLine < 1 || endColumn < 1) return;
            if (textEditor.Document == null) return;
            try
            {
                var startOffset = GetOffset(textEditor.Document, startLine, startColumn);
                var endOffset = GetOffset(textEditor.Document, endLine, endColumn + 1);
                endOffset = Math.Min(endOffset, textEditor.Document.TextLength);
                if (endOffset > startOffset)
                {
                    spans.Add(new SymbolSpan(startOffset, endOffset, kind));
                }
            }
            catch
            {
                // Ignore invalid spans
            }
        }

        static int GetOffset(TextDocument document, int line, int column)
        {
            var documentLine = document.GetLineByNumber(line);
            var cappedColumn = Math.Clamp(column - 1, 0, documentLine.Length);
            return documentLine.Offset + cappedColumn;
        }

        void ApplySymbolSpans(IReadOnlyList<SymbolSpan> spans)
        {
            if (symbolColorizer != null)
            {
                textEditor.TextArea.TextView.LineTransformers.Remove(symbolColorizer);
            }

            symbolColorizer = spans.Count == 0 ? null : new SymbolColorizer(spans);

            if (symbolColorizer != null)
            {
                textEditor.TextArea.TextView.LineTransformers.Add(symbolColorizer);
            }

            textEditor.TextArea.TextView.InvalidateVisual();
        }

        void ClearSymbolColorizer()
        {
            if (symbolColorizer != null)
            {
                textEditor.TextArea.TextView.LineTransformers.Remove(symbolColorizer);
                symbolColorizer = null;
                textEditor.TextArea.TextView.InvalidateVisual();
            }
        }

        private void LoadHighlighting()
        {
            using var reader = XmlReader.Create("Nen.xshd");
            textEditor.SyntaxHighlighting = HighlightingLoader.Load(reader, HighlightingManager.Instance);
        }

        private (int startOffset, string currentWord) GetCurrentWord()
        {
            var caretOffset = textEditor.TextArea.Caret.Offset;
            var doc = textEditor.Document;
            var startOffset = caretOffset;

            while (startOffset > 0)
            {
                var c = doc.GetCharAt(startOffset - 1);
                if (char.IsLetterOrDigit(c) || c == '_')
                {
                    startOffset--;
                    continue;
                }
                break;
            }

            var length = caretOffset - startOffset;
            var currentWord = length > 0 ? doc.GetText(startOffset, length) : string.Empty;
            return (startOffset, currentWord);
        }

        enum SymbolKind
        {
            Class,
            Method,
            Field,
            Parameter,
            Local
        }

        readonly record struct SymbolSpan(int StartOffset, int EndOffset, SymbolKind Kind);

        sealed class SymbolColorizer : DocumentColorizingTransformer
        {
            readonly IReadOnlyList<SymbolSpan> spans;
            readonly IReadOnlyDictionary<SymbolKind, Brush> brushes;

            public SymbolColorizer(IReadOnlyList<SymbolSpan> spans)
            {
                this.spans = spans;
                brushes = new Dictionary<SymbolKind, Brush>
                {
                    { SymbolKind.Class, CreateBrush(38, 127, 153) },
                    { SymbolKind.Method, CreateBrush(121, 94, 38) },
                    { SymbolKind.Field, CreateBrush(0, 16, 128) },
                    { SymbolKind.Parameter, CreateBrush(0, 16, 128) },
                    { SymbolKind.Local, CreateBrush(0, 16, 128) }
                };
            }

            protected override void ColorizeLine(DocumentLine line)
            {
                var lineStart = line.Offset;
                var lineEnd = line.EndOffset;
                foreach (var span in spans)
                {
                    if (span.EndOffset <= lineStart || span.StartOffset >= lineEnd) continue;
                    var start = Math.Max(span.StartOffset, lineStart);
                    var end = Math.Min(span.EndOffset, lineEnd);
                    if (end <= start) continue;
                    ChangeLinePart(start, end, element =>
                    {
                        if (brushes.TryGetValue(span.Kind, out var brush))
                        {
                            element.TextRunProperties.SetForegroundBrush(brush);
                        }
                        if (span.Kind is SymbolKind.Class or SymbolKind.Method)
                        {
                            element.TextRunProperties.SetTypeface(CreateTypeface(element.TextRunProperties, FontWeights.SemiBold));
                        }
                    });
                }
            }

            static Brush CreateBrush(byte r, byte g, byte b)
            {
                var brush = new SolidColorBrush(Color.FromRgb(r, g, b));
                brush.Freeze();
                return brush;
            }

            static Typeface CreateTypeface(VisualLineElementTextRunProperties properties, FontWeight fontWeight)
            {
                var current = properties.Typeface;
                return new Typeface(current.FontFamily, current.Style, fontWeight, current.Stretch);
            }
        }
    }
}
