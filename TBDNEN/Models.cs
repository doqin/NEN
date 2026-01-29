using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TBDNEN
{
    namespace Models
    {
        class DuAnNen
        {
            public required string tên { get; set; }
            public required string[] nguồn { get; set; }
            public required string đích { get; set; }
        }
    }
}
