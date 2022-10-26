import java.net.*;
import java.io.*;
 
public class server {
  public static void main(String args[]) throws Exception {
    ServerSocket s = new ServerSocket(6768);
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
    
            int n = socketIn.readInt();
            System.out.println("Am primit numarul: " + n);
    
            socketOut.writeInt(n);
            System.out.println("Am trimis numarul: " + n);
            
            socketOut.flush();
            System.out.println("Am terminat thredul nr " + k);
            c.close();
        }
        catch(Exception e) {

        }
    }
    
}