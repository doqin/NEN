using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.CodeCompletion;
using ICSharpCode.AvalonEdit.Highlighting;
using ICSharpCode.AvalonEdit.Highlighting.Xshd;
using ICSharpCode.AvalonEdit.Search;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Xml;

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
        CompletionWindow? completionWindow;

        public EditorControl()
        {
            InitializeComponent();
            textEditor.TextChanged += (_, __) =>
            {
                if (Text != textEditor.Text)
                    Text = textEditor.Text;
            };
            textEditor.TextArea.TextEntered += textEditor_TextArea_TextEntered;
            textEditor.TextArea.TextEntering += textEditor_TextArea_TextEntering;
            SearchPanel.Install(textEditor.TextArea);
            LoadHighlighting();
        }

        void textEditor_TextArea_TextEntered(object sender, TextCompositionEventArgs e)
        {
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
    }
}
