using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.CodeCompletion;
using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Highlighting;
using ICSharpCode.AvalonEdit.Highlighting.Xshd;
using ICSharpCode.AvalonEdit.Rendering;
using ICSharpCode.AvalonEdit.Search;
using Microsoft.Win32;
using NEN;
using NEN.AST;
using NEN.Exceptions;
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
using static NEN.Lexer;

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
        NEN.Symbols.Symbol[] symbolCompletions = [];

        public EditorControl()
        {
            InitializeComponent();
            textEditor.Options.ConvertTabsToSpaces = true;
            textEditor.Options.IndentationSize = 4;
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
            foreach (var keyword in NEN.Lexer.Keywords)
            {
                data.Add(new Types.NENCompletionData(keyword));
            }
            foreach (var symbol in symbolCompletions)
            {
                data.Add(new Types.NENCompletionData(symbol switch
                {
                    NEN.Symbols.MethodBase methodBase => methodBase.Name,
                    NEN.Symbols.FieldSymbol fieldSymbol => fieldSymbol.Name,
                    NEN.Symbols.LocalSymbol localSymbol => localSymbol.Name,
                    NEN.Symbols.ClassSymbol classSymbol => classSymbol.Name,
                    NEN.Symbols.ANSISymbol ansiSymbol => ansiSymbol.Name,
                    _ => throw new NotImplementedException(),
                }));
            }

            var normalizedWord = Types.NENCompletionData.Normalize(currentWord);
            if (!string.IsNullOrEmpty(normalizedWord))
            {
                completionWindow.CompletionList.SelectItem(normalizedWord);
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
                var (symbols, spans) = module.CollectSymbols(FilePath!, textEditor.Document);
                symbolCompletions = [.. symbols];
                ApplySymbolSpans(spans);
            }
            catch (NENException) // only catch NENExceptions, if a regular exception happens we gotta fix it
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

        void ApplySymbolSpans(IReadOnlyList<NEN.Symbols.SymbolSpan> spans)
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

        static bool IsSystemDarkTheme()
        {
            try
            {
                using var personalize = Registry.CurrentUser.OpenSubKey("Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize");
                var raw = personalize?.GetValue("AppsUseLightTheme");
                var value = raw is int i ? i : raw is byte b ? b : (int?)null;
                // 0 = dark, 1 = light
                return value.HasValue && value.Value == 0;
            }
            catch
            {
                return false;
            }
        }

        private void LoadHighlighting()
        {
            using var reader = XmlReader.Create("Nen.xshd");
            var highlighting = HighlightingLoader.Load(reader, HighlightingManager.Instance);
            if (IsSystemDarkTheme())
            {
                ApplyDarkThemeHighlighting(highlighting);
            }
            textEditor.SyntaxHighlighting = highlighting;
        }

        static void ApplyDarkThemeHighlighting(IHighlightingDefinition highlighting)
        {
            var palette = new Dictionary<string, Color>
            {
                { "Keyword", Color.FromRgb(86, 156, 214) },
                { "Comment", Color.FromRgb(87, 166, 74) },
                { "String", Color.FromRgb(214, 157, 133) },
                { "Marker", Color.FromRgb(78, 201, 176) },
                { "Number", Color.FromRgb(181, 206, 168) }
            };

            foreach (var color in highlighting.NamedHighlightingColors)
            {
                if (palette.TryGetValue(color.Name, out var brushColor))
                {
                    color.Foreground = new SimpleHighlightingBrush(brushColor);
                }
            }
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

        sealed class SymbolColorizer : DocumentColorizingTransformer
        {
            readonly IReadOnlyList<NEN.Symbols.SymbolSpan> spans;
            readonly IReadOnlyDictionary<NEN.Symbols.SymbolKind, Brush> brushes;

            public SymbolColorizer(IReadOnlyList<NEN.Symbols.SymbolSpan> spans)
            {
                this.spans = spans;
                var isDarkTheme = IsSystemDarkTheme();
                brushes = isDarkTheme ? CreateDarkThemeBrushes() : CreateLightThemeBrushes();
            }

            static Dictionary<NEN.Symbols.SymbolKind, Brush> CreateLightThemeBrushes() => new()
            {
                { NEN.Symbols.SymbolKind.Class, CreateBrush(38, 127, 153) },
                { NEN.Symbols.SymbolKind.Method, CreateBrush(121, 94, 38) },
                { NEN.Symbols.SymbolKind.Field, CreateBrush(0, 16, 128) },
                { NEN.Symbols.SymbolKind.Parameter, CreateBrush(0, 16, 128) },
                { NEN.Symbols.SymbolKind.Local, CreateBrush(0, 16, 128) }
            };

            static Dictionary<NEN.Symbols.SymbolKind, Brush> CreateDarkThemeBrushes() => new()
            {
                { NEN.Symbols.SymbolKind.Class, CreateBrush(78, 201, 176) },
                { NEN.Symbols.SymbolKind.Method, CreateBrush(220, 220, 170) },
                { NEN.Symbols.SymbolKind.Field, CreateBrush(220, 220, 220) },
                { NEN.Symbols.SymbolKind.Parameter, CreateBrush(156, 220, 254) },
                { NEN.Symbols.SymbolKind.Local, CreateBrush(156, 220, 254) }
            };

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
                        if (!IsSystemDarkTheme())
                        {
                            if (span.Kind is NEN.Symbols.SymbolKind.Class or NEN.Symbols.SymbolKind.Method or NEN.Symbols.SymbolKind.Field)
                            {
                                element.TextRunProperties.SetTypeface(CreateTypeface(element.TextRunProperties, FontWeights.SemiBold));
                            }
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
