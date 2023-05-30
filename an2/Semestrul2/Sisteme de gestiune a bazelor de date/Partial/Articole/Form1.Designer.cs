namespace Articole
{
    partial class Form1
    {
        /// <summary>
        ///  Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        ///  Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        ///  Required method for Designer support - do not modify
        ///  the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.Windows.Forms.Label labelPret;
            this.buttonUpdate = new System.Windows.Forms.Button();
            this.labelDescriere = new System.Windows.Forms.Label();
            this.labelDenumire = new System.Windows.Forms.Label();
            this.labelCantitate = new System.Windows.Forms.Label();
            this.textBoxGen = new System.Windows.Forms.TextBox();
            this.textBoxTitulatura = new System.Windows.Forms.TextBox();
            this.textBoxPrenume = new System.Windows.Forms.TextBox();
            this.textBoxNume = new System.Windows.Forms.TextBox();
            this.buttonAdd = new System.Windows.Forms.Button();
            this.buttonDelete = new System.Windows.Forms.Button();
            this.dataGridViewChild = new System.Windows.Forms.DataGridView();
            this.dataGridViewParent = new System.Windows.Forms.DataGridView();
            this.buttonConnect = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.textBoxDataNasterii = new System.Windows.Forms.TextBox();
            labelPret = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewChild)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewParent)).BeginInit();
            this.SuspendLayout();
            // 
            // labelPret
            // 
            labelPret.AutoSize = true;
            labelPret.Location = new System.Drawing.Point(454, 17);
            labelPret.Name = "labelPret";
            labelPret.Size = new System.Drawing.Size(72, 20);
            labelPret.TabIndex = 28;
            labelPret.Text = "Titulatura";
            // 
            // buttonUpdate
            // 
            this.buttonUpdate.BackColor = System.Drawing.Color.WhiteSmoke;
            this.buttonUpdate.Location = new System.Drawing.Point(567, 80);
            this.buttonUpdate.Name = "buttonUpdate";
            this.buttonUpdate.Size = new System.Drawing.Size(138, 43);
            this.buttonUpdate.TabIndex = 30;
            this.buttonUpdate.Text = "Update";
            this.buttonUpdate.UseVisualStyleBackColor = false;
            this.buttonUpdate.Click += new System.EventHandler(this.buttonUpdate_Click);
            // 
            // labelDescriere
            // 
            this.labelDescriere.AutoSize = true;
            this.labelDescriere.Location = new System.Drawing.Point(595, 17);
            this.labelDescriere.Name = "labelDescriere";
            this.labelDescriere.Size = new System.Drawing.Size(35, 20);
            this.labelDescriere.TabIndex = 29;
            this.labelDescriere.Text = "Gen";
            // 
            // labelDenumire
            // 
            this.labelDenumire.AutoSize = true;
            this.labelDenumire.Location = new System.Drawing.Point(312, 17);
            this.labelDenumire.Name = "labelDenumire";
            this.labelDenumire.Size = new System.Drawing.Size(67, 20);
            this.labelDenumire.TabIndex = 27;
            this.labelDenumire.Text = "Prenume";
            // 
            // labelCantitate
            // 
            this.labelCantitate.AutoSize = true;
            this.labelCantitate.Location = new System.Drawing.Point(167, 17);
            this.labelCantitate.Name = "labelCantitate";
            this.labelCantitate.Size = new System.Drawing.Size(49, 20);
            this.labelCantitate.TabIndex = 26;
            this.labelCantitate.Text = "Nume";
            // 
            // textBoxGen
            // 
            this.textBoxGen.Location = new System.Drawing.Point(595, 40);
            this.textBoxGen.Name = "textBoxGen";
            this.textBoxGen.Size = new System.Drawing.Size(125, 27);
            this.textBoxGen.TabIndex = 25;
            // 
            // textBoxTitulatura
            // 
            this.textBoxTitulatura.Location = new System.Drawing.Point(454, 40);
            this.textBoxTitulatura.Name = "textBoxTitulatura";
            this.textBoxTitulatura.Size = new System.Drawing.Size(125, 27);
            this.textBoxTitulatura.TabIndex = 24;
            // 
            // textBoxPrenume
            // 
            this.textBoxPrenume.Location = new System.Drawing.Point(312, 40);
            this.textBoxPrenume.Name = "textBoxPrenume";
            this.textBoxPrenume.Size = new System.Drawing.Size(125, 27);
            this.textBoxPrenume.TabIndex = 23;
            // 
            // textBoxNume
            // 
            this.textBoxNume.Location = new System.Drawing.Point(167, 40);
            this.textBoxNume.Name = "textBoxNume";
            this.textBoxNume.Size = new System.Drawing.Size(125, 27);
            this.textBoxNume.TabIndex = 22;
            // 
            // buttonAdd
            // 
            this.buttonAdd.BackColor = System.Drawing.Color.WhiteSmoke;
            this.buttonAdd.Location = new System.Drawing.Point(259, 80);
            this.buttonAdd.Name = "buttonAdd";
            this.buttonAdd.Size = new System.Drawing.Size(138, 43);
            this.buttonAdd.TabIndex = 20;
            this.buttonAdd.Text = "Add";
            this.buttonAdd.UseVisualStyleBackColor = false;
            this.buttonAdd.Click += new System.EventHandler(this.buttonAdd_Click);
            // 
            // buttonDelete
            // 
            this.buttonDelete.BackColor = System.Drawing.Color.WhiteSmoke;
            this.buttonDelete.Location = new System.Drawing.Point(414, 80);
            this.buttonDelete.Name = "buttonDelete";
            this.buttonDelete.Size = new System.Drawing.Size(138, 43);
            this.buttonDelete.TabIndex = 21;
            this.buttonDelete.Text = "Delete";
            this.buttonDelete.UseVisualStyleBackColor = false;
            this.buttonDelete.Click += new System.EventHandler(this.buttonDelete_Click);
            // 
            // dataGridViewChild
            // 
            this.dataGridViewChild.AllowUserToAddRows = false;
            this.dataGridViewChild.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dataGridViewChild.Location = new System.Drawing.Point(495, 142);
            this.dataGridViewChild.Name = "dataGridViewChild";
            this.dataGridViewChild.ReadOnly = true;
            this.dataGridViewChild.RowHeadersWidth = 51;
            this.dataGridViewChild.RowTemplate.Height = 29;
            this.dataGridViewChild.Size = new System.Drawing.Size(468, 470);
            this.dataGridViewChild.TabIndex = 19;
            // 
            // dataGridViewParent
            // 
            this.dataGridViewParent.AllowUserToAddRows = false;
            this.dataGridViewParent.AllowUserToDeleteRows = false;
            this.dataGridViewParent.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dataGridViewParent.Location = new System.Drawing.Point(12, 142);
            this.dataGridViewParent.MultiSelect = false;
            this.dataGridViewParent.Name = "dataGridViewParent";
            this.dataGridViewParent.ReadOnly = true;
            this.dataGridViewParent.RowHeadersWidth = 51;
            this.dataGridViewParent.RowTemplate.Height = 29;
            this.dataGridViewParent.Size = new System.Drawing.Size(466, 470);
            this.dataGridViewParent.TabIndex = 18;
            this.dataGridViewParent.CellClick += new System.Windows.Forms.DataGridViewCellEventHandler(this.dataGridViewParent_CellClick);
            // 
            // buttonConnect
            // 
            this.buttonConnect.BackColor = System.Drawing.Color.WhiteSmoke;
            this.buttonConnect.Location = new System.Drawing.Point(418, 634);
            this.buttonConnect.Name = "buttonConnect";
            this.buttonConnect.Size = new System.Drawing.Size(149, 61);
            this.buttonConnect.TabIndex = 17;
            this.buttonConnect.Text = "Connect to DB";
            this.buttonConnect.UseVisualStyleBackColor = false;
            this.buttonConnect.Click += new System.EventHandler(this.buttonConnect_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(726, 17);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(92, 20);
            this.label1.TabIndex = 32;
            this.label1.Text = "DataNasterii";
            // 
            // textBoxDataNasterii
            // 
            this.textBoxDataNasterii.Location = new System.Drawing.Point(726, 40);
            this.textBoxDataNasterii.Name = "textBoxDataNasterii";
            this.textBoxDataNasterii.Size = new System.Drawing.Size(125, 27);
            this.textBoxDataNasterii.TabIndex = 31;
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 20F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(988, 718);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.textBoxDataNasterii);
            this.Controls.Add(this.buttonUpdate);
            this.Controls.Add(this.labelDescriere);
            this.Controls.Add(labelPret);
            this.Controls.Add(this.labelDenumire);
            this.Controls.Add(this.labelCantitate);
            this.Controls.Add(this.textBoxGen);
            this.Controls.Add(this.textBoxTitulatura);
            this.Controls.Add(this.textBoxPrenume);
            this.Controls.Add(this.textBoxNume);
            this.Controls.Add(this.buttonAdd);
            this.Controls.Add(this.buttonDelete);
            this.Controls.Add(this.dataGridViewChild);
            this.Controls.Add(this.dataGridViewParent);
            this.Controls.Add(this.buttonConnect);
            this.Name = "Form1";
            this.Text = "Form1";
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewChild)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridViewParent)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private Button buttonUpdate;
        private Label labelDescriere;
        private Label labelDenumire;
        private Label labelCantitate;
        private TextBox textBoxGen;
        private TextBox textBoxTitulatura;
        private TextBox textBoxPrenume;
        private TextBox textBoxNume;
        private Button buttonAdd;
        private Button buttonDelete;
        private DataGridView dataGridViewChild;
        private DataGridView dataGridViewParent;
        private Button buttonConnect;
        private Label label1;
        private TextBox textBoxDataNasterii;
    }
}