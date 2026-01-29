using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.Highlighting;
using ICSharpCode.AvalonEdit.Highlighting.Xshd;
using System.Windows;
using System.Windows.Controls;
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

        public EditorControl()
        {
            InitializeComponent();
            textEditor.TextChanged += (_, __) =>
            {
                if (Text != textEditor.Text)
                    Text = textEditor.Text;
            };
            LoadHighlighting();
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
    }
}
