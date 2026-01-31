using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.Highlighting;
using ICSharpCode.AvalonEdit.Highlighting.Xshd;
using Microsoft.Win32;
using System.ComponentModel;
using System.DirectoryServices;
using System.IO;
using System.Linq.Expressions;
using System.Text;
using System.Text.Json;
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
using System.Diagnostics;
using ICSharpCode.AvalonEdit.CodeCompletion;

namespace TANG
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        readonly OpenFolderDialog openFolderDialog = new();
        readonly SaveFileDialog saveFileDialog = new();
        EditorTabModal editorTabViewModel;
        string workingDirectory;

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
                explorer.Items.Clear();
                AddTreeItem(explorer, path);
                workingDirectory = path;
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
                    editorTabViewModel.Add(editorTab, Path.GetFileName(file), file);
                };
                item.Items.Add(ite);
            }
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            editorTabViewModel = new EditorTabModal();
            editorTab.ItemsSource = editorTabViewModel.Tabs;
            openFolderDialog.ShowHiddenItems = true;
            var saveCommand = new RoutedCommand();
            saveCommand.InputGestures.Add(new KeyGesture(Key.S, ModifierKeys.Control));
            CommandBindings.Add(new CommandBinding(saveCommand, SaveCommand_Executed));
        }

        private void TabHeader_MouseDown(object sender, MouseButtonEventArgs e)
        {
        }

        private void Close_Click(object sender, RoutedEventArgs e)
        {
            editorTabViewModel.Remove(((TextBlock)((StackPanel)((Control)sender).Parent).Children[0]).Text); // Terrible code :P
        }

        private void Save_Click(object sender, RoutedEventArgs e)
        {
            e.Handled = true;
            if (editorTab.SelectedItem is not EditorTabItem currentItem) return;
            var path = currentItem.Path;
            var text = currentItem.Text;
            File.WriteAllText(path, text);
            MessageBox.Show("Đã lưu tệp");
        }

        private void SaveCommand_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            Save_Click(sender, e);
        }

        private void Build_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                TBDNEN.Program.Build(workingDirectory);
                explorer.Items.Clear();
                AddTreeItem(explorer, workingDirectory);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void Run_Click(object sender, RoutedEventArgs e)
        {
            string projFile = "";
            try
            {
                projFile = File.ReadAllText(Path.Combine(workingDirectory, "duannen.json"));
            }
            catch (Exception)
            {
                throw new("Không tìm thấy tệp duannen.json");
            }
            TBDNEN.Models.DuAnNen? projMetadata = null;
            try
            {
                projMetadata = JsonSerializer.Deserialize<TBDNEN.Models.DuAnNen>(projFile);
            }
            catch (Exception)
            {
                throw new("Tệp duannen.json không định dạng đúng cú pháp");
            }
            var targetDir = projMetadata!.đích;
            var assemblyName = projMetadata!.tên;
            var psi = new ProcessStartInfo
            {
                FileName = "powershell.exe",
                Arguments = $"-NoExit -Command \"dotnet '{Path.Combine(workingDirectory, targetDir, $"{assemblyName}.dll")}'\"",
                WorkingDirectory = workingDirectory,
                CreateNoWindow = false,
                UseShellExecute = false
            };
            Process.Start(psi);
        }

        private void AddFile_Click(object sender, RoutedEventArgs e)
        {
            var result = saveFileDialog.ShowDialog();
            if (result == true)
            {
                File.Create(saveFileDialog.FileName);
                explorer.Items.Clear();
                AddTreeItem(explorer, workingDirectory);
            }
        }
    }
}