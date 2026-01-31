using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.Highlighting;
using ICSharpCode.AvalonEdit.Highlighting.Xshd;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Controls;
using System.Xml;

namespace TANG.Modal
{
    internal class EditorTabItem
    {
        public required string Header { get; set; }
        public required string Text { get; set; }
        public required string Path { get; set; }
    }
    internal class EditorTabModal
    {
        public ObservableCollection<EditorTabItem> Tabs { get; set; }

        public EditorTabModal()
        {
            Tabs = [];
        }

        public void Add(TabControl tabControl, string tabName, string path)
        {
            if (Tabs.Any(t => t.Header == tabName)) return;
            var content = File.ReadAllText(path);
            var tabItem = new EditorTabItem
            {
                Header = tabName,
                Text = content,
                Path = path
            };
            Tabs.Add(tabItem);
            tabControl.SelectedItem = tabItem;
        }

        public void Remove(string tabName)
        {
            Tabs.Remove(Tabs.First(t => t.Header == tabName));
        }
    }
}
