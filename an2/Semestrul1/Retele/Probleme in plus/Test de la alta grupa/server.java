import java.net.*;
import java.util.Random;
import java.io.*;
import java.time.LocalDateTime;  // Import the LocalDateTime class
import java.time.format.DateTimeFormatter;  // Import the DateTimeFormatter 
 
public class server {
  public static void main(String args[]) throws Exception {
    ServerSocket s = new ServerSocket(12300);
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
            byte[] b = new byte[n];

            socketIn.read(b, 0, n);

            String sir  = new String(b);
            System.out.println("Am primit : " + sir + " de la thredul " + k);
            System.out.println(sir.length());

            int p1,p2,p3;
            Random r = new Random(111);
            p1 = r.nextInt(sir.length() - 2);
            System.out.println(p1);
            p2 = r.nextInt(sir.length() - 2 - p1) + p1 + 1;
            System.out.println(p2);
            p3 = r.nextInt(sir.length() - 1 - p2) + p2 + 1; 
            System.out.println(p3);

            int sum = sir.charAt(p1) +  sir.charAt(p2) + sir.charAt(p3);    
            System.out.println(p1 + " " + p2 + " " + p3 + " \nSuma cautata este : " + sum + "\n" + "de la thredul " + k);
            
            int guess = -1, guess_count = 0;
                while(true)
                {
                    guess = socketIn.readInt();
                    System.out.println("Am primit guessul: " + guess + " de la thredul " + k);

                    if(guess == sum)
                    {
                        socketOut.writeInt(guess_count);
                        System.out.println("Am trimis: " + guess_count);
                        break;
                    }


                    if(++guess_count == 5)
                    {
                        socketOut.writeInt(-2);
                        
                        int raspuns = socketIn.readInt();
                        if(raspuns == 0)
                            break;
                        else
                            continue;
                    } 

                    socketOut.writeInt(-1);
                }
            

            socketOut.flush();
            System.out.println("Am terminat thredul nr " + k);
            c.close();
        }
        catch(Exception e) {

        }
    }
    
}