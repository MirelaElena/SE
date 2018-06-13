/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package exempluinterfataprolog;

import java.io.IOException;
import java.io.InputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingUtilities;

/**
 *
 * @author Irina
 */
public class CititorMesaje extends Thread {

    ServerSocket servs;
    volatile Socket s = null;//volatile ca sa fie protejat la accesul concurent al mai multor threaduri
    volatile PipedInputStream pis = null;
    ConexiuneProlog conexiune;

    //setteri sincronizati
    public synchronized void setSocket(Socket _s) {
        s = _s;
        notify();
    }

    public final synchronized void setPipedInputStream(PipedInputStream _pis) {
        pis = _pis;
        notify();
    }
    //getteri sincronizati

    public synchronized Socket getSocket() throws InterruptedException {
        if (s == null) {
            wait();//asteapta pana este setat un socket
        }
        return s;
    }

    public synchronized PipedInputStream getPipedInputStream() throws InterruptedException {
        if (pis == null) {
            wait();
        }
        return pis;
    }

    //constructor
    public CititorMesaje(ConexiuneProlog _conexiune, ServerSocket _servs) throws IOException {
        servs = _servs;
        conexiune = _conexiune;
    }

    @Override
    public void run() {
        try {
            //apel blocant, asteapta conexiunea
            //conexiunea clinetului se face din prolog
            Socket s_aux = servs.accept();
            setSocket(s_aux);
            //pregatesc InputStream-ul pentru a citi de pe Socket
            InputStream is = s_aux.getInputStream();

            PipedOutputStream pos = new PipedOutputStream();
            setPipedInputStream(new PipedInputStream(pos, 10000000));//leg un pipedInputStream de capatul in care se scrie

            int chr;
            String str = "";
            while ((chr = is.read()) != -1) {//pana nu citeste EOF
                pos.write(chr);//pun date in Pipe, primite de la Prolog
                str += (char) chr;
                if (chr == '\n') {
                    final String sirDeScris = str;
                    str = "";
                    SwingUtilities.invokeLater(new Runnable() {
                        public void run() {
                            conexiune.getFereastra().getDebugTextArea().append(sirDeScris);
                            String text = sirDeScris.trim();
                            System.out.println("Text citit e: " + text);
//                            if (text.length() > 3 && text.charAt(0) == 'd' && text.charAt(1) == '(' && text.charAt(text.length() - 1) == ')') {
//                                String intrebare = text.substring(2, text.length() - 1);
//                                String[] cuvinte = intrebare.split("#");
//
//                                try {
//                                    conexiune.getFereastra().setImagine(cuvinte[0], cuvinte[1]);
//                                } catch (IOException ex) {
//                                    Logger.getLogger(CititorMesaje.class.getName()).log(Level.SEVERE, null, ex);
//                                }
//                            }
//                            else 
                            //verific daca e intrebare
                            if(text.length()>2 && text.charAt(0)=='i'&& text.charAt(1)=='(' && text.charAt(text.length()-1)==')')
                            {
                                String intrebare=text.substring(2, text.length()-1);
                                conexiune.getFereastra().setIntrebare(intrebare);
                            } //verific daca sunt optiuni
                            else if (text.length() > 2 && text.charAt(0) == '(' && text.charAt(text.length() - 1) == ')') {
                                System.out.println("Am intrat in setOptiuni");
                                conexiune.getFereastra().setOptiuni(text);
                            
                            }//verific daca este meniu secundar
                            else if(text.length()>2 && text.charAt(0)=='m'&& text.charAt(1)=='(' && text.charAt(text.length()-1)==')')
                            {
                                System.out.println("Meniu secundar!!!");
                                conexiune.getFereastra().setMeniuSecundar(text);
                            }                            
                            //verific daca e solutie
                            else if (text.length() > 2 && text.charAt(0) == 's' && text.charAt(1) == '(' && text.charAt(text.length() - 1) == ')') {
                                    System.out.println("Am primit solutia: " + text);
                                    String solutie = text.substring(2, text.length() - 1);
                                try {
                                    conexiune.getFereastra().setSolutie(solutie);
                                } catch (IOException ex) {
                                    Logger.getLogger(CititorMesaje.class.getName()).log(Level.SEVERE, null, ex);
                                }
                            }
                            //verific daca e primul fapt/nu exista fapte
                             else if (text.length() > 2 && text.charAt(0) == 'n' && text.charAt(1) == '(' && text.charAt(text.length() - 1) == ')') {
                                    System.out.println("Am primit primul fapt: " + text);
                                    String fapt = text.substring(2, text.length() - 1);
                               
                                    conexiune.getFereastra().setPrimulFapt(fapt);
                            }
                             //verific daca e un fapt
                             else if (text.length() > 2 && text.charAt(0) == 'f' && text.charAt(1) == '(' && text.charAt(text.length() - 1) == ')') {
                                    System.out.println("Am primit primul fapt: " + text);
                                    String fapt = text.substring(2, text.length() - 1);
                               
                                    conexiune.getFereastra().setFapt(fapt);
                            }
                        }

                    });
                }
            }

        } catch (IOException ex) {
            Logger.getLogger(CititorMesaje.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

}
