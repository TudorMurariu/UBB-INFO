using System.Data;
using System.Data.SqlClient;

namespace Briose
{
    public partial class Form1 : Form
    {
        SqlConnection cs = new SqlConnection("Data Source = DESKTOP-DV079PR\\SQLEXPRESS;" +
            " Initial Catalog = Briose; Integrated Security = True");
        SqlDataAdapter da = new SqlDataAdapter();
        BindingSource bsP = new BindingSource();
        BindingSource bsC = new BindingSource();
        DataSet dsP = new DataSet();
        DataSet dsC = new DataSet();
        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {

        }

        private void buttonConnect_Click(object sender, EventArgs e)
        {
            da.SelectCommand = new SqlCommand("SELECT * FROM Cofetarie", cs);
            dsP.Clear();
            da.Fill(dsP);
            dataGridViewParent.DataSource = dsP.Tables[0];
            bsP.DataSource = dsP.Tables[0];

            bsP.MoveLast();
        }

        private void dataGridViewParent_CellClick(object sender, DataGridViewCellEventArgs e)
        {
            if (dataGridViewParent.Rows[e.RowIndex].Cells[e.ColumnIndex].Value == null)
                return;


            string Id_Cofetarie = dataGridViewParent.Rows[e.RowIndex].Cells[0].Value.ToString();



            da.SelectCommand = new SqlCommand("SELECT * from Briosa " +
                    "where Briosa.idC = " + Id_Cofetarie + "; ", cs);
            dsC.Clear();
            da.Fill(dsC);
            dataGridViewChild.DataSource = dsC.Tables[0];
            bsC.DataSource = dsC.Tables[0];
        }
    }
}