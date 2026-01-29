using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.Highlighting;
using ICSharpCode.AvalonEdit.Highlighting.Xshd;
using Microsoft.Win32;
using System.ComponentModel;
using System.DirectoryServices;
using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Xml;
using TANG.Modal;

namespace TANG
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private readonly OpenFolderDialog openFolderDialog = new();
        private EditorTabModal editorTabViewModel;

        public MainWindow()
        {
            InitializeComponent();
        }

        private void Open_Click(object sender, RoutedEventArgs e)
        {
            var result = openFolderDialog.ShowDialog();
            if (result == true)
            {
                var path = openFolderDialog.FolderName;
                AddTreeItem(explorer, path);
            }
        }

        private void AddTreeItem<T>(T item, string path) where T : ItemsControl
        {
            var directories = Directory.GetDirectories(path);
            var files = Directory.GetFiles(path);
            foreach (var dir in directories)
            {
                var ite = new TreeViewItem
                {
                    Header = Path.GetRelativePath(path, dir),
                };
                item.Items.Add(ite);
                AddTreeItem(ite, dir);
            }
            foreach (var file in files)
            {
                var ite = new TreeViewItem
                {
                    Header = Path.GetFileName(file),
                };
                ite.MouseDoubleClick += (o, e) =>
                {
                    editorTabViewModel.Add(Path.GetFileName(file), file);
                };
                item.Items.Add(ite);
            }
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            editorTabViewModel = new EditorTabModal();
            editorTab.ItemsSource = editorTabViewModel.Tabs;
        }

        private void TabHeader_MouseDown(object sender, MouseButtonEventArgs e)
        {
        }

        private void Close_Click(object sender, RoutedEventArgs e)
        {
            editorTabViewModel.Remove(((TextBlock)((StackPanel)((Control)sender).Parent).Children[0]).Text); // Terrible code :P
        }
    }
}