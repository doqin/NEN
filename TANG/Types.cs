using ICSharpCode.AvalonEdit.CodeCompletion;
using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Editing;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TANG
{
    namespace Types
    {
        public class NENCompletionData(string text) : ICompletionData
        {
            public System.Windows.Media.ImageSource? Image
            {
                get { return null; }
            }

            public string Text { get; private set; } = text;

            public object Content
            {
                get { return this.Text; }
            }

            public object Description
            {
                get { return "Miêu tả cho " + this.Text; }
            }

            public void Complete(TextArea textArea, ISegment completionSegment,
                EventArgs insertionRequestEventArgs)
            {
                textArea.Document.Replace(completionSegment, this.Text);
            }

            public double Priority
            {
                get { return 0; }
            }
        }
    }
}
