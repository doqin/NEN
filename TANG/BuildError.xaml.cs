using NEN.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using static System.Net.Mime.MediaTypeNames;

namespace TANG
{
    /// <summary>
    /// Interaction logic for BuildError.xaml
    /// </summary>
    public partial class BuildError : Window
    {
        // public static readonly DependencyProperty TextProperty = DependencyProperty.Register(nameof(Text), typeof(string), typeof(EditorControl), new PropertyMetadata(""));

        public string Text
        {
            get => run.Text;
            set => run.Text = value;
        }

        public BuildError()
        {
            InitializeComponent();
        }
    }
}
