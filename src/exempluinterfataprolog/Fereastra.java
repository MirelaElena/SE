/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package exempluinterfataprolog;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 *
 * @author Irina
 */
public class Fereastra extends javax.swing.JFrame {

    /**
     * Creates new form Fereastra
     *
     * @param titlu
     */
    JButton butonReguli;
    JButton butonSolutii;
    JTextField incarcaReguli;
    JTextField incarcaSolutii;
    ConexiuneProlog conexiune;
    Intrebare_intrebatoare panou_intrebari;

    public Fereastra(String titlu) {
        super(titlu);
        //panou_intrebari = new Intrebare_intrebatoare();
        initComponents();

    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        grupBtn = new javax.swing.ButtonGroup();
        jPanel3 = new javax.swing.JPanel();
        jButton7 = new javax.swing.JButton();
        jButton6 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();
        jButton4 = new javax.swing.JButton();
        jButton5 = new javax.swing.JButton();
        jButton1 = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        jPanel4 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        textAreaDebug = new javax.swing.JTextArea();
        jPanel5 = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        jButton7.setText("Incarca");
        jButton7.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton7ActionPerformed(evt);
            }
        });

        jButton6.setText("Consulta");
        jButton6.setEnabled(false);
        jButton6.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton6ActionPerformed(evt);
            }
        });

        jButton3.setText("Reinitiaza");
        jButton3.setEnabled(false);
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        jButton4.setText("Cum");
        jButton4.setEnabled(false);

        jButton5.setText("Afisare_fapte");
        jButton5.setEnabled(false);

        jButton1.setBackground(new java.awt.Color(255, 102, 102));
        jButton1.setText("Iesire");
        jButton1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton1ActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 371, Short.MAX_VALUE)
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 135, Short.MAX_VALUE)
        );

        textAreaDebug.setColumns(20);
        textAreaDebug.setRows(5);
        jScrollPane1.setViewportView(textAreaDebug);

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addGap(17, 17, 17)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 272, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(20, Short.MAX_VALUE))
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addGap(20, 20, 20)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addComponent(jButton7)
                .addGap(58, 58, 58)
                .addComponent(jButton6)
                .addGap(46, 46, 46)
                .addComponent(jButton3)
                .addGap(47, 47, 47)
                .addComponent(jButton4)
                .addGap(55, 55, 55)
                .addComponent(jButton5)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jButton1)
                .addGap(16, 16, 16))
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(27, Short.MAX_VALUE))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGap(24, 24, 24)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButton7)
                    .addComponent(jButton6)
                    .addComponent(jButton3)
                    .addComponent(jButton4)
                    .addComponent(jButton5)
                    .addComponent(jButton1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 1167, Short.MAX_VALUE)
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 157, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 813, Short.MAX_VALUE)
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 209, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(42, 42, 42)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(12, 12, 12)
                        .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(0, 321, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(311, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed
        try {
            conexiune.expeditor.trimiteMesajSicstus("comanda(reinitiaza)");

        } catch (InterruptedException ex) {
            Logger.getLogger(Fereastra.class.getName()).log(Level.SEVERE, null, ex);
        }
    }//GEN-LAST:event_jButton3ActionPerformed

    private void jButton7ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton7ActionPerformed
        jPanel2.setLayout(new GridLayout(2, 2, 20, 20));
        incarcaReguli = new JTextField("'reguli_carte.txt'");
        incarcaSolutii = new JTextField("'solutii.txt'");
        butonReguli = new JButton("Incarca Regulile");
        butonReguli.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Fereastra.AFISAT_SOLUTII = false;
                String valoareParametru = incarcaReguli.getText();
                incarcaReguli.setEnabled(false);
                String dir = System.getProperty("user.dir");
                dir = dir.replace("\\", "/");
                try {
                    conexiune.expeditor.trimiteMesajSicstus("director('" + dir + "')");
                    conexiune.expeditor.trimiteMesajSicstus("incarca(" + valoareParametru + ")");

                } catch (InterruptedException ex) {
                    Logger.getLogger(Fereastra.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        });
        butonSolutii = new JButton("Incarca Solutiile");
        butonSolutii.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Fereastra.AFISAT_SOLUTII = false;
                String valoareParametru = incarcaSolutii.getText();
                incarcaSolutii.setEnabled(false);
                String dir = System.getProperty("user.dir");
                dir = dir.replace("\\", "/");
                try {
                    conexiune.expeditor.trimiteMesajSicstus("director('" + dir + "')");
                    conexiune.expeditor.trimiteMesajSicstus("incarca_sol(" + valoareParametru + ")");

                } catch (InterruptedException ex) {
                    Logger.getLogger(Fereastra.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        });
        jPanel2.add(incarcaReguli);
        jPanel2.add(butonReguli);
        jPanel2.add(incarcaSolutii);
        jPanel2.add(butonSolutii);
        jButton6.setEnabled(true);
        jPanel2.repaint();
        jPanel2.revalidate();
    }//GEN-LAST:event_jButton7ActionPerformed

    private void jButton1ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton1ActionPerformed
        try {
            conexiune.expeditor.trimiteMesajSicstus("comanda(iesire)");
            conexiune.expeditor.trimiteMesajSicstus("halt.");
        } catch (InterruptedException ex) {
            Logger.getLogger(Fereastra.class.getName()).log(Level.SEVERE, null, ex);
        }
        System.exit(0);
    }//GEN-LAST:event_jButton1ActionPerformed

    private void jButton6ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton6ActionPerformed
        jPanel2.remove(this.butonReguli);
        jPanel2.remove(this.incarcaReguli);
        jPanel2.remove(this.incarcaSolutii);
        jPanel2.remove(this.butonSolutii);
        

        this.setLayout(new FlowLayout());
        this.panou_intrebari = new Intrebare_intrebatoare();
        this.add(this.panou_intrebari);
        this.panou_intrebari.paint(null);
        this.panou_intrebari.revalidate();
        this.repaint();
        this.revalidate();
        try {
            conexiune.expeditor.trimiteMesajSicstus("comanda(consulta)");

        } catch (InterruptedException ex) {
            Logger.getLogger(Fereastra.class.getName()).log(Level.SEVERE, null, ex);
        }
        jButton3.setEnabled(true);
        jButton4.setEnabled(true);
        jButton5.setEnabled(true);
        jButton7.setEnabled(false);
        

    }//GEN-LAST:event_jButton6ActionPerformed

    private void optiuneDeleteButtonActionPerformed(java.awt.event.ActionEvent evt) {

        String raspuns = ((JButton) (evt.getSource())).getText();
        if (raspuns.equalsIgnoreCase("nu")) {//TODO: De schimbat felul cum arata interfata
            System.out.println("Trebuie sa sterg tot");
           
            this.panou_intrebari.removeAll();
            this.panou_intrebari.repaint();
            this.panou_intrebari.revalidate();
            jPanel5.removeAll();
            jPanel5.repaint();
            jPanel5.revalidate();
        }
        
        try {
            conexiune.expeditor.trimiteSirSicstus(raspuns);

        } catch (InterruptedException ex) {
            Logger.getLogger(Fereastra.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    private void optiuneButtonActionPerformed(java.awt.event.ActionEvent evt) {

        String raspuns = ((JButton) (evt.getSource())).getText();        
        try {
            conexiune.expeditor.trimiteSirSicstus(raspuns);

        } catch (InterruptedException ex) {
            Logger.getLogger(Fereastra.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        /* Set the Nimbus look and feel */
        //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
        /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
         * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html 
         */
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (ClassNotFoundException ex) {
            java.util.logging.Logger.getLogger(Fereastra.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            java.util.logging.Logger.getLogger(Fereastra.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            java.util.logging.Logger.getLogger(Fereastra.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(Fereastra.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
        //</editor-fold>

        /* Create and display the form */
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new Fereastra("Verificare").setVisible(true);

            }
        });
    }

    public javax.swing.JTextArea getDebugTextArea() {
        return textAreaDebug;
    }

    public void setConexiune(ConexiuneProlog _conexiune) {
        conexiune = _conexiune;
    }

    public void setIntrebare(String intreb) {
        this.panou_intrebari.label_intrebare.setText("<html><body style='width:100%'>" + intreb + "</html>");
        this.panou_intrebari.repaint();

    }

    public void setImagine(String cale, String descriere) throws IOException {
        /* jTextField4.setText(descriere);
        BufferedImage bf;
        bf = ImageIO.read(new File(cale));
        jLabel1.setIcon(new ImageIcon(bf));
        jPanel1.repaint();
        jPanel1.revalidate();*/
    }

    public void setOptiuni(String optiuni) {
        System.out.println("optiuni: " + optiuni);
        this.panou_intrebari.panou_optiuni.removeAll();
        this.panou_intrebari.panou_optiuni.setLayout(new FlowLayout());
        optiuni = optiuni.trim();
        optiuni = optiuni.substring(2, optiuni.length() - 1);
        optiuni = optiuni.trim();
        String[] vect_opt = optiuni.split(" ");
        for (int i = 0; i < vect_opt.length; i++) {
            System.out.println("vect_opt:" + i + ", optiuni: " + vect_opt[i]);
        }
        for (int i = 0; i < vect_opt.length; i++) {
            JButton b = new JButton(vect_opt[i]);
            b.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    optiuneButtonActionPerformed(evt);
                }
            });
            this.panou_intrebari.panou_optiuni.add(b);
        }
        this.panou_intrebari.panou_optiuni.repaint();
        this.panou_intrebari.panou_optiuni.revalidate();
        //this.revalidate();
    }

    public void setSolutie(String solutie) throws IOException {
        //if (!Fereastra.AFISAT_SOLUTII) {
        this.panou_intrebari.removeAll();
        this.panou_intrebari.setLayout(new BoxLayout(panou_intrebari, BoxLayout.Y_AXIS));
        Fereastra.AFISAT_SOLUTII = true;
        //}

        System.out.println("Solutia in getSolutie:" + solutie);

        if (solutie.length() >= 20) // solutie completa
        {//
            String[] vect_solutie = solutie.split("#");
            String textSolutie = "Locul excursiei este " + vect_solutie[0] + " cu factorul de certitudine " + vect_solutie[1] + ".";
            String imgSolutie = vect_solutie[2];
            String descSolutie = "Descriere: " + vect_solutie[3].substring(1, vect_solutie[3].length() - 1);
            String propsSolutie = "Proprietati: " + vect_solutie[4] + ".";

            String cale = imgSolutie.substring(1, imgSolutie.length() - 1) + ".jpg";
            BufferedImage bf;
            bf = ImageIO.read(new File(cale));

            JLabel jsol = new JLabel(textSolutie);
            JLabel jdesc = new JLabel(descSolutie);
            JLabel jprops = new JLabel(propsSolutie);
            JLabel jimg = new JLabel();
            jimg.setIcon(new ImageIcon(bf));
            jimg.setMaximumSize(new Dimension(100, 100));

            this.panou_intrebari.add(jsol);
            this.panou_intrebari.add(jdesc);
            this.panou_intrebari.add(jprops);
            this.panou_intrebari.add(jimg);
        } else //sumar
        {
            String textSolutie = "Locul excursiei este " + solutie + ".";
            JLabel jsol = new JLabel(textSolutie);
            this.panou_intrebari.add(jsol);
        }

        this.panou_intrebari.repaint();
        this.panou_intrebari.revalidate();
        //this.revalidate();
    }

    public static boolean AFISAT_SOLUTII = false;

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup grupBtn;
    private javax.swing.JButton jButton1;
    private javax.swing.JButton jButton3;
    private javax.swing.JButton jButton4;
    private javax.swing.JButton jButton5;
    private javax.swing.JButton jButton6;
    private javax.swing.JButton jButton7;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea textAreaDebug;
    // End of variables declaration//GEN-END:variables

    void setMeniuSecundar(String text) {
        jPanel5.removeAll();
        jPanel5.setLayout(new FlowLayout());
        JLabel labelReafiseaza = new JLabel("Reafiseaza?");
        jPanel5.add(labelReafiseaza);
        text = text.trim();
        text = text.substring(2, text.length() - 1);
        text = text.trim();
        String[] vect_meniu_sec = text.split("#");
        for (int i = 0; i < vect_meniu_sec.length; i++) {
            System.out.println("Optiune[" + i + "]=" + vect_meniu_sec[i]);
            JButton b = new JButton(vect_meniu_sec[i]);
            b.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    optiuneDeleteButtonActionPerformed(evt);
                }
            });
            jPanel5.add(b);
        }
        jPanel5.repaint();
        jPanel5.revalidate();
        //this.revalidate();
        //throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
