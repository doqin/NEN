using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.Highlighting;
using ICSharpCode.AvalonEdit.Highlighting.Xshd;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Controls;
using System.Xml;

namespace TANG.Modal
{
    internal class EditorTabItem : INotifyPropertyChanged
    {
        string header = string.Empty;
        string text = string.Empty;
        string path = string.Empty;
        string lastSavedText = string.Empty;
        bool isDirty;

        public required string Header
        {
            get => header;
            set
            {
                if (header == value) return;
                header = value;
                OnPropertyChanged(nameof(Header));
                OnPropertyChanged(nameof(DisplayHeader));
            }
        }

        public required string Text
        {
            get => text;
            set
            {
                if (text == value) return;
                text = value;
                OnPropertyChanged(nameof(Text));
                IsDirty = text != lastSavedText;
            }
        }

        public required string Path
        {
            get => path;
            set
            {
                if (path == value) return;
                path = value;
                OnPropertyChanged(nameof(Path));
            }
        }

        public string LastSavedText
        {
            get => lastSavedText;
            set
            {
                if (lastSavedText == value) return;
                lastSavedText = value;
                IsDirty = text != lastSavedText;
            }
        }

        public bool IsDirty
        {
            get => isDirty;
            private set
            {
                if (isDirty == value) return;
                isDirty = value;
                OnPropertyChanged(nameof(IsDirty));
                OnPropertyChanged(nameof(DisplayHeader));
            }
        }

        public string DisplayHeader => IsDirty ? $"* {Header}" : Header;

        public event PropertyChangedEventHandler? PropertyChanged;

        void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
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
                Path = path,
                LastSavedText = content
            };
            Tabs.Add(tabItem);
            tabControl.SelectedItem = tabItem;
        }

        public void Remove(EditorTabItem tab)
        {
            Tabs.Remove(tab);
        }
    }
}
