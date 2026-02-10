using ICSharpCode.AvalonEdit.CodeCompletion;
using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Editing;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TANG
{
    namespace Types
    {
        public class NENCompletionData(string text) : ICompletionData
        {
            readonly string insertText = text;

            public System.Windows.Media.ImageSource? Image
            {
                get { return null; }
            }

            public string Text { get; private set; } = Normalize(text);

            public object Content
            {
                get { return insertText; }
            }

            public object Description
            {
                get { return "Miêu tả cho " + insertText; }
            }

            public void Complete(TextArea textArea, ISegment completionSegment,
                EventArgs insertionRequestEventArgs)
            {
                textArea.Document.Replace(completionSegment, insertText);
            }

            public double Priority
            {
                get { return 0; }
            }

            internal static string Normalize(string text)
            {
                if (string.IsNullOrEmpty(text)) return string.Empty;

                var normalized = text.Normalize(NormalizationForm.FormD);
                var sb = new StringBuilder(normalized.Length);
                foreach (var c in normalized)
                {
                    var category = CharUnicodeInfo.GetUnicodeCategory(c);
                    if (category != UnicodeCategory.NonSpacingMark)
                    {
                        sb.Append(c);
                    }
                }

                return sb.ToString().Normalize(NormalizationForm.FormC);
            }
        }
    }
}
