using Microsoft.VisualBasic;
using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Text.Json;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using TANG.Modal;
using System.Diagnostics;
using MahApps.Metro.IconPacks;
using NEN.Exceptions;

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
        string? lastActiveTabPath;
        readonly string settingsFilePath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "TANG", "settings.json");
        EditorTabItem? draggedTab;
        Point dragStartPoint;

        public MainWindow()
        {
            InitializeComponent();
            Closing += MainWindow_Closing;
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
                var stackPanel = new StackPanel
                {
                    Orientation = Orientation.Horizontal,
                };
                var folderIcon = new PackIconRemixIcon
                {
                    Kind = PackIconRemixIconKind.Folder6Line,
                    Foreground = Brushes.Gold,
                    Padding = new Thickness(1),
                    VerticalAlignment = VerticalAlignment.Center,
                    Margin = new Thickness(0, 0, 5, 0)
                };
                var folderName = new TextBlock { 
                    Text = Path.GetRelativePath(path, dir),
                    Margin = new Thickness(5, 0, 5, 0),
                    VerticalAlignment = VerticalAlignment.Center,
                };
                stackPanel.Children.Add(folderIcon);
                stackPanel.Children.Add(folderName);
                var ite = new TreeViewItem
                {
                    Header = stackPanel,
                    Tag = dir,
                    Padding = new(1),
                    Margin = new(0)
                };
                ite.ContextMenu = BuildItemContextMenu(ite);
                item.Items.Add(ite);
                AddTreeItem(ite, dir);
            }
            foreach (var file in files)
            {
                var stackPanel = new StackPanel
                {
                    Orientation = Orientation.Horizontal,
                };
                var fileIcon = new PackIconRemixIcon
                {
                    Kind = PackIconRemixIconKind.File2Line,
                    Padding = new Thickness(1),
                    VerticalAlignment = VerticalAlignment.Center,
                    Margin = new Thickness(0, 0, 5, 0),
                };
                var fileName = new TextBlock
                {
                    Text = Path.GetFileName(file),
                    VerticalAlignment = VerticalAlignment.Center,
                    Margin = new Thickness(5, 0, 5, 0)
                };
                if (fileName.Text.EndsWith(".nen"))
                {
                    var nenIcon = new TextBlock
                    {
                        Text = "NEN",
                        Foreground = Brushes.MediumPurple,
                        FontStretch = FontStretches.UltraCondensed,
                        FontSize = 10,
                        FontWeight = FontWeights.Bold,
                        VerticalAlignment = VerticalAlignment.Center,
                        Margin = new Thickness(0, 0, 5, 0)
                    };
                    stackPanel.Children.Add(nenIcon);
                }
                else
                {
                    stackPanel.Children.Add(fileIcon);
                }
                stackPanel.Children.Add(fileName);
                var ite = new TreeViewItem
                {
                    Header = stackPanel,
                    Tag = file,
                    Padding = new(1),
                    Margin = new(0)
                };
                ite.ContextMenu = BuildItemContextMenu(ite);
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
            editorTab.SelectionChanged += EditorTab_SelectionChanged;
            editorTab.AllowDrop = true;
            editorTab.PreviewMouseLeftButtonDown += EditorTab_PreviewMouseLeftButtonDown;
            editorTab.PreviewMouseMove += EditorTab_PreviewMouseMove;
            editorTab.Drop += EditorTab_Drop;
            editorTab.DragOver += EditorTab_DragOver;
            openFolderDialog.ShowHiddenItems = true;
            var saveCommand = new RoutedCommand();
            saveCommand.InputGestures.Add(new KeyGesture(Key.S, ModifierKeys.Control));
            CommandBindings.Add(new CommandBinding(saveCommand, SaveCommand_Executed));
            LoadSettings();
        }

        private void TabHeader_MouseDown(object sender, MouseButtonEventArgs e)
        {
        }

        private void Close_Click(object sender, RoutedEventArgs e)
        {
            if ((sender as Control)?.DataContext is EditorTabItem tab)
            {
                editorTabViewModel.Remove(tab);
            }
        }

        private void Save_Click(object sender, RoutedEventArgs e)
        {
            e.Handled = true;
            if (editorTab.SelectedItem is not EditorTabItem currentItem) return;
            var path = currentItem.Path;
            var text = currentItem.Text;
            File.WriteAllText(path, text);
            currentItem.LastSavedText = text;
        }

        private void SaveCommand_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            Save_Click(sender, e);
        }

        private void Build_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                Build.IsEnabled = false;
                Cursor = Cursors.Wait;
                TBDNEN.Program.Build(workingDirectory);
                explorer.Items.Clear();
                AddTreeItem(explorer, workingDirectory);
            }
            catch (AggregateException ex)
            {
                var buildError = new BuildError
                {
                    Text = string.Join("\n", ex.InnerExceptions.Select(i => i.Message))
                };
                buildError.Show();
            }
            finally
            {
                Cursor = Cursors.Arrow;
                Build.IsEnabled = true;
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
            CreateFileInDirectory(workingDirectory);
        }

        private void AddFolder_Click(object sender, RoutedEventArgs e)
        {
            CreateFolderInDirectory(workingDirectory);
        }

        private ContextMenu BuildItemContextMenu(TreeViewItem item)
        {
            var menu = new ContextMenu();
            var renameItem = new MenuItem { Header = "Đổi tên" };
            renameItem.Click += (o, e) => RenameTreeViewItem(item);
            menu.Items.Add(renameItem);
            var deleteItem = new MenuItem { Header = "Xóa" };
            deleteItem.Click += (o, e) => DeleteTreeViewItem(item);
            menu.Items.Add(deleteItem);

            if (item.Tag is string path && Directory.Exists(path))
            {
                menu.Items.Add(new Separator());

                var addFileItem = new MenuItem { Header = "Tạo tệp mới" };
                addFileItem.Click += (o, e) => CreateFileInDirectory(path);
                menu.Items.Add(addFileItem);

                var addFolderItem = new MenuItem { Header = "Tạo thư mục" };
                addFolderItem.Click += (o, e) => CreateFolderInDirectory(path);
                menu.Items.Add(addFolderItem);
            }
            return menu;
        }

        private void RenameTreeViewItem(TreeViewItem item)
        {
            if (string.IsNullOrEmpty(workingDirectory)) return;
            if (item.Tag is not string oldPath || string.IsNullOrWhiteSpace(oldPath)) return;

            var currentName = Path.GetFileName(oldPath);
            var newName = Interaction.InputBox("Nhập tên mới", "Đổi tên", currentName).Trim();
            if (string.IsNullOrWhiteSpace(newName)) return;
            if (string.Equals(newName, currentName, StringComparison.OrdinalIgnoreCase)) return;
            if (newName.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0)
            {
                MessageBox.Show("Tên không hợp lệ.");
                return;
            }

            var parent = Path.GetDirectoryName(oldPath);
            if (string.IsNullOrEmpty(parent)) return;
            var newPath = Path.Combine(parent, newName);

            if (File.Exists(newPath) || Directory.Exists(newPath))
            {
                MessageBox.Show("Tệp hoặc thư mục với tên này đã tồn tại.");
                return;
            }

            var isDirectory = Directory.Exists(oldPath);
            try
            {
                if (isDirectory)
                {
                    Directory.Move(oldPath, newPath);
                }
                else if (File.Exists(oldPath))
                {
                    File.Move(oldPath, newPath);
                }
                else
                {
                    MessageBox.Show("Không tìm thấy tệp hoặc thư mục để đổi tên.");
                    return;
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
                return;
            }

            UpdateTabsAfterRename(oldPath, newPath, isDirectory);
            explorer.Items.Clear();
            AddTreeItem(explorer, workingDirectory);
        }

        private void UpdateTabsAfterRename(string oldPath, string newPath, bool isDirectory)
        {
            if (editorTabViewModel?.Tabs is null) return;

            if (isDirectory)
            {
                var oldPrefix = Path.TrimEndingDirectorySeparator(oldPath) + Path.DirectorySeparatorChar;
                foreach (var tab in editorTabViewModel.Tabs)
                {
                    if (tab.Path.StartsWith(oldPrefix, StringComparison.OrdinalIgnoreCase) || string.Equals(tab.Path, oldPath, StringComparison.OrdinalIgnoreCase))
                    {
                        var relative = Path.GetRelativePath(oldPath, tab.Path);
                        var newTabPath = Path.Combine(newPath, relative);
                        tab.Path = newTabPath;
                        tab.Header = Path.GetFileName(newTabPath);
                    }
                }
            }
            else
            {
                foreach (var tab in editorTabViewModel.Tabs)
                {
                    if (string.Equals(tab.Path, oldPath, StringComparison.OrdinalIgnoreCase))
                    {
                        tab.Path = newPath;
                        tab.Header = Path.GetFileName(newPath);
                    }
                }
            }

            if (!string.IsNullOrWhiteSpace(lastActiveTabPath))
            {
                if (isDirectory)
                {
                    var oldPrefix = Path.TrimEndingDirectorySeparator(oldPath) + Path.DirectorySeparatorChar;
                    if (lastActiveTabPath.StartsWith(oldPrefix, StringComparison.OrdinalIgnoreCase) || string.Equals(lastActiveTabPath, oldPath, StringComparison.OrdinalIgnoreCase))
                    {
                        var relative = Path.GetRelativePath(oldPath, lastActiveTabPath);
                        lastActiveTabPath = Path.Combine(newPath, relative);
                    }
                }
                else if (string.Equals(lastActiveTabPath, oldPath, StringComparison.OrdinalIgnoreCase))
                {
                    lastActiveTabPath = newPath;
                }
            }
        }

        private void DeleteTreeViewItem(TreeViewItem item)
        {
            if (item.Tag is not string path || string.IsNullOrWhiteSpace(path)) return;

            var isDirectory = Directory.Exists(path);
            var isFile = File.Exists(path);

            if (!isDirectory && !isFile)
            {
                MessageBox.Show("Không tìm thấy tệp hoặc thư mục để xóa.");
                return;
            }

            var typeLabel = isDirectory ? "thư mục" : "tệp";
            var name = Path.GetFileName(path);
            var confirm = MessageBox.Show($"Bạn có chắc muốn xóa {typeLabel} '{name}'?", "Xác nhận", MessageBoxButton.YesNo, MessageBoxImage.Warning);
            if (confirm != MessageBoxResult.Yes) return;

            try
            {
                if (isDirectory)
                {
                    Directory.Delete(path, true);
                }
                else
                {
                    File.Delete(path);
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
                return;
            }

            if (editorTabViewModel?.Tabs is not null && isFile)
            {
                var tab = editorTabViewModel.Tabs.FirstOrDefault(t => string.Equals(t.Path, path, StringComparison.OrdinalIgnoreCase));
                if (tab != null)
                {
                    editorTabViewModel.Tabs.Remove(tab);
                }
            }

            if (!string.IsNullOrWhiteSpace(lastActiveTabPath))
            {
                if (isDirectory)
                {
                    var oldPrefix = Path.TrimEndingDirectorySeparator(path) + Path.DirectorySeparatorChar;
                    if (lastActiveTabPath.StartsWith(oldPrefix, StringComparison.OrdinalIgnoreCase) || string.Equals(lastActiveTabPath, path, StringComparison.OrdinalIgnoreCase))
                    {
                        lastActiveTabPath = null;
                    }
                }
                else if (string.Equals(lastActiveTabPath, path, StringComparison.OrdinalIgnoreCase))
                {
                    lastActiveTabPath = null;
                }
            }

            explorer.Items.Clear();
            AddTreeItem(explorer, workingDirectory);
        }

        private void CreateFileInDirectory(string targetDirectory)
        {
            if (string.IsNullOrWhiteSpace(targetDirectory)) return;

            var fileName = Interaction.InputBox("Nhập tên tệp mới", "Tạo tệp", "Tệp mới.txt").Trim();
            if (string.IsNullOrWhiteSpace(fileName)) return;
            if (fileName.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0)
            {
                MessageBox.Show("Tên tệp không hợp lệ.");
                return;
            }

            var filePath = Path.Combine(targetDirectory, fileName);
            if (File.Exists(filePath) || Directory.Exists(filePath))
            {
                MessageBox.Show("Tên đã tồn tại.");
                return;
            }

            try
            {
                using (File.Create(filePath)) { }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
                return;
            }

            explorer.Items.Clear();
            AddTreeItem(explorer, workingDirectory);
        }

        private void CreateFolderInDirectory(string targetDirectory)
        {
            if (string.IsNullOrWhiteSpace(targetDirectory)) return;

            var folderName = Interaction.InputBox("Nhập tên thư mục mới", "Tạo thư mục", "Thư mục mới").Trim();
            if (string.IsNullOrWhiteSpace(folderName)) return;
            if (folderName.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0)
            {
                MessageBox.Show("Tên thư mục không hợp lệ.");
                return;
            }

            var newFolderPath = Path.Combine(targetDirectory, folderName);
            if (Directory.Exists(newFolderPath) || File.Exists(newFolderPath))
            {
                MessageBox.Show("Tên đã tồn tại.");
                return;
            }

            try
            {
                Directory.CreateDirectory(newFolderPath);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
                return;
            }

            explorer.Items.Clear();
            AddTreeItem(explorer, workingDirectory);
        }

        private void EditorTab_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            lastActiveTabPath = (editorTab.SelectedItem as EditorTabItem)?.Path;
        }

        private void MainWindow_Closing(object? sender, CancelEventArgs e)
        {
            SaveSettings();
        }

        private void LoadSettings()
        {
            try
            {
                if (!File.Exists(settingsFilePath)) return;
                var json = File.ReadAllText(settingsFilePath);
                var settings = JsonSerializer.Deserialize<UserSettings>(json);
                if (settings is null) return;

                if (!string.IsNullOrWhiteSpace(settings.WorkingDirectory) && Directory.Exists(settings.WorkingDirectory))
                {
                    workingDirectory = settings.WorkingDirectory;
                    explorer.Items.Clear();
                    AddTreeItem(explorer, workingDirectory);
                }

                if (!string.IsNullOrWhiteSpace(settings.LastActiveTabPath) && File.Exists(settings.LastActiveTabPath))
                {
                    editorTabViewModel.Add(editorTab, Path.GetFileName(settings.LastActiveTabPath), settings.LastActiveTabPath);
                }

                if (settings.OpenTabPaths is not null)
                {
                    foreach (var tabPath in settings.OpenTabPaths)
                    {
                        if (File.Exists(tabPath))
                        {
                            editorTabViewModel.Add(editorTab, Path.GetFileName(tabPath), tabPath);
                        }
                    }
                }

                lastActiveTabPath = settings.LastActiveTabPath;

                if (!string.IsNullOrWhiteSpace(lastActiveTabPath))
                {
                    var tab = editorTabViewModel.Tabs.FirstOrDefault(t => string.Equals(t.Path, lastActiveTabPath, StringComparison.OrdinalIgnoreCase));
                    if (tab is not null)
                    {
                        editorTab.SelectedItem = tab;
                    }
                }
            }
            catch (Exception)
            {
                // Ignore settings load errors to avoid blocking startup
            }
        }

        private void SaveSettings()
        {
            try
            {
                Directory.CreateDirectory(Path.GetDirectoryName(settingsFilePath)!);
                var settings = new UserSettings
                {
                    WorkingDirectory = workingDirectory,
                    LastActiveTabPath = lastActiveTabPath,
                    OpenTabPaths = editorTabViewModel?.Tabs?.Select(t => t.Path).ToList()
                };
                var json = JsonSerializer.Serialize(settings);
                File.WriteAllText(settingsFilePath, json);
            }
            catch (Exception)
            {
                // Ignore save errors on exit
            }
        }

        private void EditorTab_PreviewMouseLeftButtonDown(object sender, MouseButtonEventArgs e)
        {
            dragStartPoint = e.GetPosition(null);
            draggedTab = GetTabItemFromPoint(e.GetPosition(editorTab));
        }

        private void EditorTab_PreviewMouseMove(object sender, MouseEventArgs e)
        {
            if (e.LeftButton != MouseButtonState.Pressed || draggedTab is null) return;

            var position = e.GetPosition(null);
            if (Math.Abs(position.X - dragStartPoint.X) > SystemParameters.MinimumHorizontalDragDistance ||
                Math.Abs(position.Y - dragStartPoint.Y) > SystemParameters.MinimumVerticalDragDistance)
            {
                DragDrop.DoDragDrop(editorTab, new DataObject(typeof(EditorTabItem), draggedTab), DragDropEffects.Move);
            }
        }

        private void EditorTab_DragOver(object sender, DragEventArgs e)
        {
            if (!e.Data.GetDataPresent(typeof(EditorTabItem)))
            {
                e.Effects = DragDropEffects.None;
            }
            else
            {
                e.Effects = DragDropEffects.Move;
            }
            e.Handled = true;
        }

        private void EditorTab_Drop(object sender, DragEventArgs e)
        {
            if (!e.Data.GetDataPresent(typeof(EditorTabItem))) return;
            var sourceTab = e.Data.GetData(typeof(EditorTabItem)) as EditorTabItem;
            if (sourceTab is null || editorTabViewModel?.Tabs is null) return;

            var targetTab = GetTabItemFromPoint(e.GetPosition(editorTab));
            if (targetTab is null || ReferenceEquals(sourceTab, targetTab)) return;

            var sourceIndex = editorTabViewModel.Tabs.IndexOf(sourceTab);
            var targetIndex = editorTabViewModel.Tabs.IndexOf(targetTab);
            if (sourceIndex < 0 || targetIndex < 0 || sourceIndex == targetIndex) return;

            editorTabViewModel.Tabs.Move(sourceIndex, targetIndex);
            editorTab.SelectedItem = sourceTab;
            lastActiveTabPath = sourceTab.Path;
        }

        private EditorTabItem? GetTabItemFromPoint(Point point)
        {
            var element = editorTab.InputHitTest(point) as DependencyObject;
            while (element != null && element is not TabItem)
            {
                element = VisualTreeHelper.GetParent(element);
            }

            if (element is TabItem tabItem)
            {
                return tabItem.DataContext as EditorTabItem;
            }
            return null;
        }
    }
}

namespace TANG
{
    internal class UserSettings
    {
        public string? WorkingDirectory { get; set; }
        public string? LastActiveTabPath { get; set; }
        public List<string>? OpenTabPaths { get; set; }
    }
}
