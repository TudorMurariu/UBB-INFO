import java.net.*;
import java.util.Random;
import java.io.*;
import java.time.LocalDateTime;  // Import the LocalDateTime class
import java.time.format.DateTimeFormatter;  // Import the DateTimeFormatter 
 
public class Server {
  public static void main(String args[]) throws Exception {
    ServerSocket s = new ServerSocket(7083);
    int n = 1;
    while(true) 
    {
        Socket c1 = s.accept();
        System.out.println("Client1 connected!");

        Socket c2 = s.accept();
        System.out.println("Client2 connected!");

        ServerThread serverThread = new ServerThread(c1, c2, n++);
        serverThread.start();
        
    }  


    //s.close();
  }
}

class ServerThread extends Thread {

    Socket c1 = null;
    Socket c2 = null;
    int k;

    ServerThread(Socket c1, Socket c2, int k) {
        this.c1 = c1;
        this.c2 = c2;
        this.k = k;
    }

    @Override
    public void run() {

        try{
            DataInputStream socketIn1 = new DataInputStream(c1.getInputStream());
            DataOutputStream socketOut1 = new DataOutputStream(c1.getOutputStream());

            DataInputStream socketIn2 = new DataInputStream(c2.getInputStream());
            DataOutputStream socketOut2 = new DataOutputStream(c2.getOutputStream());
    
            socketOut1.writeInt(k);
            socketOut1.writeInt((int)1);
            socketOut2.writeInt(k);
            socketOut2.writeInt((int)2);
            
            Random r = new Random(111);
            String alfabet = "abcdefghijklmnopqrstuvwxyz";
            char litera = alfabet.charAt(r.nextInt(alfabet.length()));
            String cuvant_ = null;

            while(true)
            {
                socketOut1.write((litera + "").getBytes(), 0, 1);

                int n =  socketIn1.readInt();
                byte[] cuvant_bytes = new byte[n];

                socketIn1.read(cuvant_bytes, 0, n);

                String cuvant = new String(cuvant_bytes);

                System.out.println("Am primit de la clientul 1 cuvantul: " + cuvant);

                byte[] litera2_byte = new byte[1];
                socketIn1.read(litera2_byte, 0, 1);
                
                String litera2 = new String(litera2_byte);
                System.out.println("Am primit de la clientul 1 litera: " + litera2);

                if(cuvant.indexOf(litera) == -1)
                {
                    n = -1;
                }

                socketOut2.writeInt(n);

                socketOut2.write(cuvant_bytes, 0, n);

                socketOut2.write(litera2_byte);


                n =  socketIn2.readInt();

                break;
            }

            socketOut2.flush();
            socketOut1.flush();
            System.out.println("Am terminat thredul nr " + k);
            c1.close();
            c2.close();
        }
        catch(Exception e) {

        }
    }
    
}