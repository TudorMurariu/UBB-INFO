import java.net.*;
import java.util.Random;
import java.io.*;
import java.time.LocalDateTime;  // Import the LocalDateTime class
import java.time.format.DateTimeFormatter;  // Import the DateTimeFormatter 
 
public class Server {
  public static void main(String args[]) throws Exception {
    ServerSocket s = new ServerSocket(12301);
    int n = 1;
    while(true) 
    {
        Socket c = s.accept();
        System.out.println("Client connected!");

        ServerThread serverThread = new ServerThread(c, n++);
        serverThread.start();
        
    }  


    //s.close();
  }
}

class ServerThread extends Thread {

    

    Socket c = null;
    int k;

    ServerThread(Socket c, int k) {
        this.c = c;
        this.k = k;
    }

    @Override
    public void run() {

        try{
            DataInputStream socketIn = new DataInputStream(c.getInputStream());
            DataOutputStream socketOut = new DataOutputStream(c.getOutputStream());
    
            int n1 = socketIn.readInt();
            byte[] b = new byte[n1];

            socketIn.read(b, 0, n1);

            String sir1  = new String(b);
            System.out.println("Am primit : " + sir1 + " de la thredul " + k);
            System.out.println(sir1.length());

            
            int n2 = socketIn.readInt();
            b = new byte[n2];

            socketIn.read(b, 0, n2);

            String sir2  = new String(b);
            System.out.println("Am primit : " + sir2 + " de la thredul " + k);
            System.out.println(sir2.length());

            StringBuilder rez = new StringBuilder(sir1.length() + sir2.length());
            int i = 0, j = 0;

            while(i < sir1.length() && j < sir2.length())
            {
                if(sir1.charAt(i) < sir2.charAt(j))
                {
                    rez.append(sir1.charAt(i));
                    i++;
                }
                else {
                    rez.append(sir2.charAt(j));
                    j++;
                }
            }

            while(i < sir1.length()) {
                rez.append(sir1.charAt(i));
                i++;
            }

            while(j < sir2.length()) {
                rez.append(sir2.charAt(j));
                j++;
            }
            
            socketOut.write(rez.toString().getBytes());

            socketOut.flush();
            System.out.println("Am terminat thredul nr " + k);
            c.close();
        }
        catch(Exception e) {

        }
    }
    
}