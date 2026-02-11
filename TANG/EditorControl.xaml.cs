using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.CodeCompletion;
using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Folding;
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
        TextMarkerService? textMarkerService;
        FoldingManager? foldingManager;
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

            textMarkerService = new TextMarkerService(textEditor);
            textEditor.TextArea.TextView.BackgroundRenderers.Add(textMarkerService);
            textEditor.MouseHover += TextEditor_MouseHover;

            SearchPanel.Install(textEditor.TextArea);
            foldingManager = FoldingManager.Install(textEditor.TextArea);
            LoadHighlighting();
            ApplyFoldingTheme();
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
                ClearFoldings();
                return;
            }

            var content = textEditor.Text;
            if (string.IsNullOrWhiteSpace(content))
            {
                ClearSymbolColorizer();
                ClearFoldings();
                return;
            }

            Module? module = null;
            try
            {
                List<ModulePart> moduleParts = [];
                var parserExceptions = new AggregateException();
                string moduleName = "Trình soạn thảo";

                if (TryLoadProjectMetadata(out var project, out var projectRoot) && project != null)
                {
                    moduleName = string.IsNullOrWhiteSpace(project.tên) ? moduleName : project.tên;
                    
                    foreach (var source in project.nguồn ?? [])
                    {
                        var resolvedPath = ResolveProjectPath(projectRoot, source);
                        if (string.IsNullOrWhiteSpace(resolvedPath)) continue;

                        Token[] tokens;

                        if (!string.IsNullOrWhiteSpace(FilePath) && string.Equals(resolvedPath, FilePath, StringComparison.OrdinalIgnoreCase))
                        {
                            tokens = Lexer.TokenizeFromText(content);
                        }
                        else if (File.Exists(resolvedPath))
                        {
                            tokens = Lexer.Tokenize(resolvedPath);
                        }
                        else
                        {
                            continue;
                        }

                        var parser = new Parser(resolvedPath, tokens);
                        var (modulePart, tempExceptions) = parser.Parse();
                        parserExceptions = new AggregateException([..parserExceptions.InnerExceptions, ..tempExceptions.InnerExceptions]);
                        moduleParts.Add(modulePart);
                    }
                }

                if (moduleParts.Count == 0)
                {
                    var tokens = Lexer.TokenizeFromText(content);
                    var parser = new Parser(FilePath ?? "Trình soạn thảo", tokens);
                    var (modulePart, tempExceptions) = parser.Parse();
                    parserExceptions = new AggregateException([.. parserExceptions.InnerExceptions, .. tempExceptions.InnerExceptions]);
                    moduleParts.Add(modulePart);
                }
                if (parserExceptions.InnerExceptions.Count > 0)
                {
                    throw parserExceptions;
                }
                var analyzer = new StaticAnalyzer(moduleName, [.. moduleParts], []);
                (module, var moduleExceptions) = analyzer.Analyze();
                if (moduleExceptions.InnerExceptions.Count > 0)
                {
                    throw moduleExceptions;
                }
                var (symbols, spans) = module.CollectSymbols(FilePath!, textEditor.Document);
                symbolCompletions = [.. symbols];
                ApplySymbolSpans(spans);
                UpdateFoldings(module);
                textMarkerService?.Clear();
            }
            catch (AggregateException ex)
            {
                if (module != null) {
                    var (symbols, spans) = module.CollectSymbols(FilePath!, textEditor.Document);
                    symbolCompletions = [.. symbols];
                    ApplySymbolSpans(spans);
                    UpdateFoldings(module);
                }
                else
                {
                    ClearFoldings();
                }
                if (textMarkerService != null)
                {
                    textMarkerService.Clear();
                    for (int i = 0; i < ex.InnerExceptions.Count; i++)
                    {
                        if (ex.InnerExceptions[i] is not NENException) throw (ex.InnerExceptions[i])!;
                        var nenEx = ex.InnerExceptions[i] as NENException;
                        if (FilePath != nenEx!.SourcePath) continue;
                        //ClearSymbolColorizer();
                        try
                        {
                            if (nenEx!.StartLine > 0 && nenEx.StartLine <= textEditor.Document.LineCount)
                            {
                                var line = textEditor.Document.GetLineByNumber(nenEx.StartLine);
                                var startOffset = line.Offset + nenEx.StartColumn;
                                var endOffset = line.Offset + nenEx.EndColumn;
                                if (startOffset < line.Offset) startOffset = line.Offset;
                                if (endOffset > line.EndOffset) endOffset = line.EndOffset;
                                if (endOffset > startOffset)
                                {
                                    textMarkerService.Add(new TextMarker(startOffset, endOffset - startOffset, nenEx.ErrorMessage));
                                }
                            }
                        }
                        catch { }
                    }
                }
            }
        }

        void UpdateFoldings(Module module)
        {
            if (foldingManager == null || textEditor.Document == null)
            {
                return;
            }

            var document = textEditor.Document;
            var sourcePath = FilePath ?? "Trình soạn thảo";
            var newFoldings = new List<NewFolding>();

            foreach (var modulePart in module.ModuleParts)
            {
                if (!string.Equals(modulePart.SourceName, sourcePath, StringComparison.OrdinalIgnoreCase))
                {
                    continue;
                }

                foreach (var classNode in modulePart.Classes)
                {
                    AddFolding(newFoldings, document, classNode.BlockStartLine, classNode.BlockEndLine);
                    foreach (var constructor in classNode.Constructors ?? [])
                    {
                        AddFolding(newFoldings, document, constructor.BlockStartLine, constructor.BlockEndLine);
                    }
                    foreach (var method in classNode.Methods)
                    {
                        AddFolding(newFoldings, document, method.BlockStartLine, method.BlockEndLine);
                    }
                }
            }

            newFoldings.Sort((left, right) => left.StartOffset.CompareTo(right.StartOffset));
            foldingManager.UpdateFoldings(newFoldings, document.TextLength);
        }

        static void AddFolding(List<NewFolding> foldings, TextDocument document, int startLine, int endLine)
        {
            if (startLine <= 0 || endLine <= startLine)
            {
                return;
            }

            if (startLine > document.LineCount || endLine > document.LineCount)
            {
                return;
            }

            var headerLine = document.GetLineByNumber(startLine);
            var startOffset = headerLine.EndOffset;
            var endOffset = document.GetLineByNumber(endLine).EndOffset;
            if (endOffset <= startOffset)
            {
                return;
            }

            foldings.Add(new NewFolding(startOffset, endOffset));
        }

        void ClearFoldings()
        {
            if (foldingManager == null || textEditor.Document == null)
            {
                return;
            }

            foldingManager.UpdateFoldings([], textEditor.Document.TextLength);
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

        void ApplyFoldingTheme()
        {
            if (!IsSystemDarkTheme())
            {
                return;
            }

            var foldingMargin = textEditor.TextArea.LeftMargins.OfType<FoldingMargin>().FirstOrDefault();
            if (foldingMargin == null)
            {
                return;
            }

            var markerBrush = new SolidColorBrush(Color.FromRgb(200, 200, 200));
            markerBrush.Freeze();
            var backgroundBrush = new SolidColorBrush(Color.FromRgb(45, 45, 48));
            backgroundBrush.Freeze();
            var selectedBackgroundBrush = new SolidColorBrush(Color.FromRgb(100, 100, 100));
            selectedBackgroundBrush.Freeze();
            foldingMargin.FoldingMarkerBrush = markerBrush;
            foldingMargin.FoldingMarkerBackgroundBrush = backgroundBrush;
            foldingMargin.SelectedFoldingMarkerBrush = markerBrush;
            foldingMargin.SelectedFoldingMarkerBackgroundBrush = selectedBackgroundBrush;
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

        void TextEditor_MouseHover(object sender, MouseEventArgs e)
        {
            var pos = textEditor.GetPositionFromPoint(e.GetPosition(textEditor));
            if (pos.HasValue)
            {
                var offset = textEditor.Document.GetOffset(pos.Value.Line, pos.Value.Column);
                var marker = textMarkerService?.GetMarkersAtOffset(offset).FirstOrDefault();
                if (marker != null)
                {
                    var toolTip = new ToolTip { Content = marker.ErrorMessage, IsOpen = true, StaysOpen = false };
                    e.Handled = true;
                }
            }
        }

        sealed class TextMarkerService : IBackgroundRenderer
        {
            readonly TextEditor textEditor;
            readonly TextSegmentCollection<TextMarker> markers;

            public TextMarkerService(TextEditor textEditor)
            {
                this.textEditor = textEditor;
                this.markers = new TextSegmentCollection<TextMarker>(textEditor.Document);
            }

            public void Add(TextMarker marker)
            {
                markers.Add(marker);
                textEditor.TextArea.TextView.Redraw(marker);
            }

            public void Clear()
            {
                var old = markers.ToArray();
                markers.Clear();
                foreach (var m in old) textEditor.TextArea.TextView.Redraw(m);
            }

            public IEnumerable<TextMarker> GetMarkersAtOffset(int offset) => markers.FindSegmentsContaining(offset);

            public KnownLayer Layer => KnownLayer.Selection;

            public void Draw(TextView textView, DrawingContext drawingContext)
            {
                if (!textView.VisualLinesValid) return;
                var visualLines = textView.VisualLines;
                if (visualLines.Count == 0) return;
                int viewStart = visualLines.First().FirstDocumentLine.Offset;
                int viewEnd = visualLines.Last().LastDocumentLine.EndOffset;

                foreach (var marker in markers.FindOverlappingSegments(viewStart, viewEnd - viewStart))
                {
                    foreach (var r in BackgroundGeometryBuilder.GetRectsForSegment(textView, marker))
                    {
                        var startPoint = r.BottomLeft;
                        var endPoint = r.BottomRight;
                        var brush = Brushes.Red;
                        var pen = new Pen(brush, 1);
                        pen.Freeze();

                        double offset = 2.5;
                        int count = Math.Max((int)((endPoint.X - startPoint.X) / offset) + 1, 4);

                        var geometry = new StreamGeometry();
                        using (var ctx = geometry.Open())
                        {
                            ctx.BeginFigure(startPoint, false, false);
                            ctx.PolyLineTo(CreatePoints(startPoint, endPoint, offset, count).ToArray(), true, false);
                        }
                        geometry.Freeze();

                        drawingContext.DrawGeometry(Brushes.Transparent, pen, geometry);
                    }
                }
            }

            static IEnumerable<Point> CreatePoints(Point start, Point end, double offset, int count)
            {
                for (int i = 0; i < count; i++)
                    yield return new Point(start.X + (i * offset), start.Y - ((i + 1) % 2 == 0 ? offset : 0));
            }
        }

        sealed class TextMarker : TextSegment
        {
            public TextMarker(int startOffset, int length, string errorMessage)
            {
                StartOffset = startOffset;
                Length = length;
                ErrorMessage = errorMessage;
            }

            public string ErrorMessage { get; }
        }
    }
}
