using System.Data;
using System.Data.SqlClient;

namespace Articole
{
    public partial class Form1 : Form
    {
        SqlConnection cs = new SqlConnection("Data Source = DESKTOP-DV079PR\\SQLEXPRESS;" +
            " Initial Catalog = S5; Integrated Security = True");
        SqlDataAdapter da = new SqlDataAdapter();
        BindingSource bsP = new BindingSource();
        BindingSource bsC = new BindingSource();
        DataSet dsP = new DataSet();
        DataSet dsC = new DataSet();
        public Form1()
        {
            InitializeComponent();
        }

        private void buttonConnect_Click(object sender, EventArgs e)
        {
            da.SelectCommand = new SqlCommand("SELECT * FROM Facultati", cs);
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


            string Fid = dataGridViewParent.Rows[e.RowIndex].Cells[0].Value.ToString();


            //Fel_De_Mancare

            da.SelectCommand = new SqlCommand("SELECT * from Profesori " +
                    "where Profesori.Fid = " + Fid + "; ", cs);
            dsC.Clear();
            da.Fill(dsC);
            dataGridViewChild.DataSource = dsC.Tables[0];
            bsC.DataSource = dsC.Tables[0];
        }

        private void buttonAdd_Click(object sender, EventArgs e)
        {
            if (dataGridViewParent.SelectedCells.Count == 0)
            {
                MessageBox.Show("O linie in parinte trebuie slectata!");
                return;
            }
            else if (dataGridViewParent.SelectedCells.Count > 1)
            {
                MessageBox.Show("O singura linie in parinte trebuie slectata!");
                return;
            }

            da.InsertCommand = new
                SqlCommand("INSERT INTO Profesori" +
                    " VALUES (@N,@P,@T,@G,@Data,@id);", cs);
            da.InsertCommand.Parameters.Add("@id",
                SqlDbType.Int).Value = dsP.Tables[dataGridViewParent.CurrentCell.ColumnIndex].Rows[dataGridViewParent.CurrentCell.RowIndex][0];

            da.InsertCommand.Parameters.Add("@N",
                SqlDbType.VarChar).Value = textBoxNume.Text;

            da.InsertCommand.Parameters.Add("@P",
                SqlDbType.VarChar).Value = textBoxPrenume.Text;

            da.InsertCommand.Parameters.Add("@T",
                SqlDbType.VarChar).Value = textBoxTitulatura.Text;

            da.InsertCommand.Parameters.Add("@G",
                SqlDbType.VarChar).Value = textBoxGen.Text;

            da.InsertCommand.Parameters.Add("@Data",
                SqlDbType.Date).Value = textBoxDataNasterii.Text;

            cs.Open();
            da.InsertCommand.ExecuteNonQuery();
            cs.Close();
            dsC.Clear();
            da.Fill(dsC);
        }

        private void buttonDelete_Click(object sender, EventArgs e)
        {
            if (dataGridViewChild.SelectedCells.Count == 0)
            {
                MessageBox.Show("O linie in copil trebuie slectata!");
                return;
            }
            else if (dataGridViewChild.SelectedCells.Count > 1)
            {
                MessageBox.Show("O singura linie in copil trebuie slectata!");
                return;
            }

            da.DeleteCommand = new SqlCommand("Delete " +
            "from Profesori where Pid = @id;", cs);

            da.DeleteCommand.Parameters.Add("@id",
                SqlDbType.Int).Value = dsC.Tables[0].Rows[dataGridViewChild.CurrentCell.RowIndex][0];

            cs.Open();
            da.DeleteCommand.ExecuteNonQuery();
            cs.Close();
            dsC.Clear();
            da.Fill(dsC);
        }

        private void buttonUpdate_Click(object sender, EventArgs e)
        {
            if (dataGridViewChild.SelectedCells.Count == 0)
            {
                MessageBox.Show("O linie in copil trebuie slectata!");
                return;
            }
            else if (dataGridViewChild.SelectedCells.Count > 1)
            {
                MessageBox.Show("O singura linie in copil trebuie slectata!");
                return;
            }

            int x;
            da.UpdateCommand = new SqlCommand("Update " +
                "Profesori set Nume = @N, Prenume = @P," +
                " Titulatura = @T, Gen = @G, DataNastere = @Data " +
                "where Pid=@id", cs);

            da.UpdateCommand.Parameters.Add("@id",
                SqlDbType.Int).Value = dsC.Tables[0].Rows[dataGridViewChild.CurrentCell.RowIndex][0];

            da.UpdateCommand.Parameters.Add("@N",
                SqlDbType.VarChar).Value = textBoxNume.Text;

            da.UpdateCommand.Parameters.Add("@P",
                SqlDbType.VarChar).Value = textBoxPrenume.Text;

            da.UpdateCommand.Parameters.Add("@T",
                SqlDbType.VarChar).Value = textBoxTitulatura.Text;

            da.UpdateCommand.Parameters.Add("@G",
                SqlDbType.VarChar).Value = textBoxGen.Text;

            da.UpdateCommand.Parameters.Add("@Data",
                SqlDbType.Date).Value = textBoxDataNasterii.Text;


            cs.Open();
            x = da.UpdateCommand.ExecuteNonQuery();
            cs.Close();
            dsC.Clear();
            da.Fill(dsC);

            if (x >= 1)
                MessageBox.Show("The record has been updated");
        }
    }
}